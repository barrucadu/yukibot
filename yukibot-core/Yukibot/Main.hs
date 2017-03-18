{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Yukibot.Main
-- Copyright   : (c) 2016 Michael Walker
-- License     : MIT
-- Stability   : experimental
-- Portability : LambdaCase, OverloadedStrings, ScopedTypeVariables
module Yukibot.Main
    ( -- * Execution
      defaultMain
    , makeBot
    , getChannelMonitors
    , getChannelCommands

    -- * Configuration
    , instantiateBackends
    , instantiateBackend
    , instantiatePlugins
    , checkCommandsAndMonitors

      -- * State
    , BotState
    , emptyBotState
      -- ** Backends
    , addBackend
    -- ** Plugins
    , addPlugin

    -- * Errors
    , CoreError(..)
    , dieWithErrors
    , formatCoreError
    , formatParseError
    ) where

import Control.Arrow ((&&&), (***))
import Control.Monad (void)
import Control.Monad.Catch (SomeException, catch)
import Data.Either (lefts, rights)
import Data.Foldable (toList)
import qualified Data.HashMap.Strict as H
import Data.List (isPrefixOf, nub)
import Data.List.NonEmpty (NonEmpty(..), nonEmpty)
import Data.Maybe (isNothing, fromMaybe, mapMaybe)
import Data.Monoid ((<>), mconcat)
import Data.Semigroup (Semigroup, sconcat)
import Data.Text (Text, unpack)
import qualified Data.Text as T
import System.Exit (die)
import System.FilePath (FilePath)
import System.Posix.Signals (Handler(..), installHandler, sigINT, sigTERM)

import Yukibot.Backend
import Yukibot.Configuration
import Yukibot.Monad
import qualified Yukibot.Plugin.Builtin as Builtin
import Yukibot.Mongo (mongoConfig)
import Yukibot.Types

-- | A default @main@ function: parse the config file and either
-- halt+report errors, or start.
defaultMain :: BotState -> FilePath -> IO ()
defaultMain st0 fp = do
  mcfg <- parseConfigFile fp
  case mcfg of
    Right cfg -> do
      builtinst <- Builtin.initialState cfg
      case makeBot st0 builtinst cfg of
        Right go   -> go
        Left  errs -> dieWithErrors formatCoreError "creating the bot" (toList errs)
    Left err ->
      let pos  = errorPos err
          line = show (sourceLine   pos)
          col  = show (sourceColumn pos)
          errs = errorMessages err
      in dieWithErrors formatParseError ("parsing the configuration file at line " <> line <> ":, column " <> col) errs

-- | Create a bot with the given state and configuration, the returned
-- action terminates when all backends are stopped. This automatically
-- sets up the \"builtin\" plugin.
makeBot :: BotState
  -- ^ The initial state of the bot.
  -> Builtin.BuiltinState
  -- ^ The initial state of the \"builtin\" plugin. This is normally
  -- supplied by 'defaultMain'.
  -> Table
  -- ^ The global configuration.
  -> Either (NonEmpty CoreError) (IO ())
makeBot st0 builtinst cfg = do
  -- Add the \"builtin\" plugin
  st <- either (Left . (:|[])) Right $
        addPlugin "builtin" (Builtin.plugin builtinst) st0
  -- Deal with the configuration
  bs <- instantiateBackends (getBackends st) (getPlugins st) cfg
  -- Return the bot action
  pure $ do -- Start all backends
    hs <- mapM startWithPlugins bs
    -- Install signal handlers to kill backends
    void $ installHandler sigINT  (Catch $ mapM_ killBackend hs) Nothing
    void $ installHandler sigTERM (Catch $ mapM_ killBackend hs) Nothing
    -- Wait for termination
    mapM_ awaitStop hs

  where
    -- Start a backend with the provided plugins.
    startWithPlugins ib = startInstantiatedBackend handle ib where
      -- Unlike in original yukibot, the plugins (for a single
      -- backend) are run in a single thread. This makes output more
      -- deterministic when multiple plugins fire on the same event,
      -- and in practice most events are only handled by one plugin,
      -- so this saves needless forking as well.
      handle ev = do
        -- Helpers to run a backend action
        let runP pn = runBackendM builtinst (mongoConfig cfg) ib (Just pn) ev
        let runNP   = runBackendM builtinst (mongoConfig cfg) ib Nothing ev

        -- First run the monitors
        monitors <- runNP (getChannelMonitors builtinst)
        mapM_ (\(pn, Monitor _ m) -> runP pn (m ev)) monitors

        -- Then the commands
        commands <- runNP (getChannelCommands builtinst)
        mapM_ (\(pn, Command _ c, args) -> runP pn (c ev args)) commands

    -- Kill a backend
    killBackend h = stopBackend h `catch` (\(_ :: SomeException) -> pure ())

-- | Return all monitors enabled in this channel.
getChannelMonitors :: Builtin.BuiltinState -> BackendM [(PluginName, Monitor)]
getChannelMonitors st = do
  ib      <- getInstance
  enabled <- Builtin.getEnabledMonitors st

  pure [ (pname, mon)
       | (pname, plugin) <- instPlugins ib
       , (mn, mon) <- H.toList $ pluginMonitors plugin
       , (pname, mn) `elem` enabled
       ]

-- | Return all commands enabled in this channel which match the given
-- message.
getChannelCommands :: Builtin.BuiltinState -> BackendM [(PluginName, Command, [Text])]
getChannelCommands st = do
  ib      <- getInstance
  ev      <- getEvent
  prefix  <- Builtin.getCommandPrefix   st
  enabled <- Builtin.getEnabledCommands st

  let UserName user = eventWhoAmI ev
  let prefixes = [prefix, user <> ": ", user <> ", ", user <> " "]

  pure $ case checkPrefixes ev prefixes of
    Just rest ->
      [ (pname, cmd, args)
      | (pname, plugin) <- instPlugins ib
      , (cn, cmd) <- H.toList $ pluginCommands plugin
      , args <- checkVerb pname cn (T.words rest) enabled
      ]
    Nothing -> []

  where
    -- Check if any of the prefixes match the message, and return
    -- suffix if so.
    checkPrefixes ev (p:ps) = case T.stripPrefix p (eventMessage ev) of
      Just rest -> Just rest
      Nothing   -> checkPrefixes ev ps
    checkPrefixes ev []
      | isNothing (eventChannel ev) = Just (eventMessage ev)
      | otherwise = Nothing

    -- Check if a message contains the verbs for a command, returning
    -- the arguments if so.
    checkVerb pn cn msg ((pn', cn', verb):rest) =
      let v = T.words verb
      in if pn == pn' && cn == cn' && v `isPrefixOf` msg
         then drop (length v) msg : checkVerb pn cn msg rest
         else checkVerb pn cn msg rest
    checkVerb _ _ _ [] = []

-------------------------------------------------------------------------------
-- Configuration

-- | Configure and instantiate all backends.
instantiateBackends :: H.HashMap BackendName (Text -> Table -> Either Text Backend)
  -- ^ The backends.
  -> H.HashMap PluginName (Table -> Either Text Plugin)
  -- ^ The plugins. This should include the \"builtin\" plugin (or at
  -- least, one with that name).
  -> Table
  -- ^ The global config.
  -> Either (NonEmpty CoreError) [InstantiatedBackend]
instantiateBackends allBackends allPlugins cfg0 = mangle id (:[]) . concat $
  [ get (BackendName n) cfgs
  | (n, VTable cfgs) <- maybe [] H.toList (getTable "backend" cfg0)
  ]
  where
    -- Instantiate all backends of the given type from the config.
    get :: BackendName
        -> Table
        -> [Either (NonEmpty CoreError) InstantiatedBackend]
    get name cfgs = case H.lookup name allBackends of
      Just b -> flip concatMap (H.toList cfgs) $ \case
        (n, VTable  c)  -> [make b name n 0 (c `override` cfg0)]
        (n, VTArray cs) -> [make b name n i (c `override` cfg0) | (i, c) <- zip [0..] (toList cs)]
        (n, _) -> [make b name n 0 cfg0]
      _ -> [Left (BackendUnknown name:|[])]

    make = instantiateBackend allPlugins

-- | Instantiate an individual backend.
instantiateBackend :: H.HashMap PluginName (Table -> Either Text Plugin)
  -- ^ The plugins. This should include the \"builtin\" plugin (or at
  -- least, one with that name).
  -> (Text -> Table -> Either Text Backend)
  -- ^ Instantiation function
  -> BackendName
  -- ^ The name of the backend.
  -> Text
  -- ^ The name of this particular instance of the backend.
  -> Int
  -- ^ The index of this particular instance of the backend.
  -> Table
  -- ^ The configuration.
  -> Either (NonEmpty CoreError) InstantiatedBackend
instantiateBackend allPlugins bf bname sname index cfg = case bf sname cfg of
  Right b  ->
    let enabledPlugins = instantiatePlugins allPlugins bname sname cfg
        -- Override the log file of the backend with values from the
        -- configuration, if present.
        b' = b { logFile = maybe (logFile b) unpack $ getString "logfile" cfg }
    in case mangle id (:[]) enabledPlugins of
         Right plugins -> case checkCommandsAndMonitors plugins bname sname cfg of
           Just errs -> Left errs
           Nothing -> Right (InstantiatedBackend bname sname index b' plugins)
         Left errs -> Left errs
  Left err -> Left (BackendBadConfig bname sname err:|[])

-- | Configure and instantiate all plugins of a backend.
--
-- The \"builtin\" plugin is automatically added to the enabled list,
-- but NOT automatically inserted into the plugin map, as it needs
-- some special set-up. This means that if you call this function
-- WITHOUT adding \"builtin\" to the map you WILL get an unknown
-- plugin error!
instantiatePlugins :: H.HashMap PluginName (Table -> Either Text Plugin)
  -- ^ The plugins.
  -> BackendName
  -- ^ The name of the backend.
  -> Text
  -- ^ The name of this particular instance of the backend.
  -> Table
  -- ^ The configuration.
  -> [Either (NonEmpty CoreError) (PluginName, Plugin)]
instantiatePlugins allPlugins bname name cfg =
  [make (PluginName n) | n <- nub $ "builtin":getStrings "plugins" cfg]
  where
    -- Instantiate a plugin.
    make :: PluginName -> Either (NonEmpty CoreError) (PluginName, Plugin)
    make n = case H.lookup n allPlugins of
      Just toP -> case toP (pcfg n) of
        Right plugin -> Right (n, plugin)
        Left err -> Left (PluginBadConfig bname name n err:|[])
      Nothing  -> Left (PluginUnknown bname name n:|[])

    -- Get the configuration of a plugin
    pcfg :: PluginName -> Table
    pcfg n = fromMaybe H.empty $ getNestedTable ["plugin", getPluginName n] cfg

-- | Check that all monitors and commands specified in the
-- configuration do actually exist.
checkCommandsAndMonitors :: [(PluginName, Plugin)]
  -- ^ The instantiated plugins.
  -> BackendName
  -- ^ The name of the backend
  -> Text
  -- ^ The name of this particular instance of the backend.
  -> Table
  -- ^ The configuration.
  -> Maybe (NonEmpty CoreError)
checkCommandsAndMonitors plugins bname sname cfg = nonEmpty (badMonitors ++ badCommands ++ badDisabled) where
  badCommands = mapMaybe checkCommand . maybe [] H.toList $ getTable "commands" cfg
  badDisabled = concatMap checkDisabled . maybe [] H.toList $ getTable "disabled" cfg
  badMonitors = mapMaybe checkMonitor $ getStrings "monitors" cfg

  -- Check a command definition for correctness.
  checkCommand (verb, VString cmd) =
    let (pname, cname) = (PluginName *** CommandName . T.drop 1) (T.breakOn ":" cmd)
    in case lookup pname plugins of
         Just plugin
           | T.null (getCommandName cname) ->
               Just (CommandMissingName bname sname pname verb)
           | null (T.words verb) ->
               Just (CommandMissingVerb bname sname pname cname)
           | cname `notElem` H.keys (pluginCommands plugin) ->
               Just (CommandUnknown bname sname pname cname verb)
           | otherwise -> Nothing
         Nothing -> Just (CommandNoSuchPlugin bname sname pname cname verb)
  checkCommand (verb, _) = Just (CommandBadFormat bname sname verb)

  -- Check a default-disabled monitor for correctness.
  checkDisabled (cname, VArray mons)
    | not $ all (\case { VString _ -> True; _ -> False }) mons =
        [DisabledBadFormat bname sname (ChannelName cname)]
    | otherwise = flip mapMaybe (toList mons) $ \(VString mon) -> case checkMonitor mon of
        Just err -> Just (DisabledError bname sname (ChannelName cname) err)
        Nothing
          | T.null cname -> Just (DisabledMissingChannel bname sname)
          | otherwise -> Nothing
  checkDisabled (cname, _) = [DisabledBadFormat bname sname (ChannelName cname)]

  -- Check a monitor definition for correctness.
  checkMonitor monitor =
    let (pname, mname) = (PluginName *** MonitorName . T.drop 1) (T.breakOn ":" monitor)
    in case lookup pname plugins of
          Just plugin
            | T.null (getMonitorName mname) ->
                Just (MonitorMissingName bname sname pname)
            | mname `notElem` H.keys (pluginMonitors plugin) ->
                Just (MonitorUnknown bname sname pname mname)
            | otherwise -> Nothing
          Nothing -> Just (MonitorNoSuchPlugin bname sname pname mname)


-------------------------------------------------------------------------------
-- State

-- | Initial state for a bot.
data BotState = BotState
  { stBackends :: H.HashMap BackendName (Text -> Table -> Either Text Backend)
  , stPlugins  :: H.HashMap PluginName  (Table -> Either Text Plugin)
  }

-- | An empty bot state: no backends, no plugins.
emptyBotState :: BotState
emptyBotState = BotState { stBackends = H.empty
                         , stPlugins  = H.empty
                         }

-- | Add a new backend.
--
-- Returns @BackendNameClash@ if there is a clash.
addBackend :: BackendName
  -- ^ The name of the backend (e.g. \"irc\")
  -> (Text -> Table -> Either Text Backend)
  -- ^ The instantiation function.
  -> BotState -> Either CoreError BotState
addBackend name backend st
  | H.member name (stBackends st) = Left (BackendNameClash name)
  | otherwise = Right st { stBackends = H.insert name backend (stBackends st) }

-- | Get the backends.
getBackends :: BotState -> H.HashMap BackendName (Text -> Table -> Either Text Backend)
getBackends = stBackends

-- | Add a new plugin.
--
-- Returns @PluginNameClash@ if there is a clash.
addPlugin :: PluginName
  -- ^ The name of the plugin (e.g. \"hello\")
  -> (Table -> Either Text Plugin)
  -- ^ The instantiation function.
  -> BotState -> Either CoreError BotState
addPlugin name plugin st
  | H.member name (stPlugins st) = Left (PluginNameClash name)
  | otherwise = Right st { stPlugins = H.insert name plugin (stPlugins st) }

-- | Get the plugins.
getPlugins :: BotState -> H.HashMap PluginName (Table -> Either Text Plugin)
getPlugins = stPlugins

-------------------------------------------------------------------------------
-- Errors

-- | Pretty-print an error list and die
dieWithErrors :: (error -> String) -> String -> [error] -> IO ()
dieWithErrors _ what [] = die ("An unknown error occurred while " <> what <> ".")
dieWithErrors showE what errs =
  let showErrs = mconcat . map (("\n  • "<>) . showE)
  in die ("An error occurred while " <> what <> ":" <> showErrs errs)

-- | Pretty-print a 'CoreError'.
formatCoreError :: CoreError -> String
formatCoreError = T.unpack . go where
  go (BackendNameClash bname) =
    "The backend " <> getBackendName bname <> " is already defined"
  go (BackendUnknown bname) =
    "Unknown backend " <> getBackendName bname
  go (BackendBadConfig bname sname err) =
    "The configuration for the backend " <> goB bname sname <> " is invalid: " <> err
  go (PluginNameClash pname) =
    "The plugin " <> getPluginName pname <> " is already defined"
  go (PluginUnknown bname sname pname) =
    "Unknown plugin " <> getPluginName pname <> " in the backend " <> goB bname sname
  go (PluginBadConfig bname sname pname err) =
    "The configuration for the plugin " <> getPluginName pname <> " in the backend " <> goB bname sname <> " is invalid: " <> err
  go (MonitorNoSuchPlugin bname sname pname mname) =
    "Unknown plugin in the monitor " <> goM pname mname <> " in the backend " <> goB bname sname
  go (MonitorMissingName bname sname _) =
    "Missing monitor name in the backend " <> goB bname sname
  go (MonitorUnknown bname sname pname mname) =
    "Unknown monitor " <> goM pname mname <> " in the backend " <> goB bname sname
  go (CommandNoSuchPlugin bname sname pname cname verb) =
    "Unknown plugin in the command " <> goC pname cname verb <> " in the backend " <> goB bname sname
  go (CommandMissingName bname sname _ _) =
    "Missing command name in the backend " <> goB bname sname
  go (CommandBadFormat bname sname _) =
    "Bad command format in the backend " <> goB bname sname
  go (CommandMissingVerb bname sname pname cname) =
    "Missing verb in the command " <> goC pname cname "" <> " in the backend " <> goB bname sname
  go (CommandUnknown bname sname pname cname verb) =
    "Unknown command " <> goC pname cname verb <> " in the backend " <> goB bname sname
  go (DisabledMissingChannel bname sname) =
    "Missing channel name in a disabled monitor declaration in the backend " <> goB bname sname
  go (DisabledBadFormat bname sname cname) =
    "Bad disabled monitor declaration for the channel " <> getChannelName cname <> " in the backend " <> goB bname sname
  go (DisabledError bname sname cname err) =
    "Bad monitor in disabled monitor declaration for the channel " <> getChannelName cname <> " in the backend " <> goB bname sname <> ": " <> go err

  goB bname sname = "'" <> getBackendName bname <> ".\"" <> sname <> "\"'"
  goM pname mname = "'" <> getPluginName pname <> ":" <> getMonitorName mname <> "'"
  goC pname cname verb = "'" <> verb <> " = " <> getPluginName pname <> ":" <> getCommandName cname <> "'"

-- | Pretty-print a parse error.
formatParseError :: Message -> String
formatParseError (SysUnExpect str) = "Unexpected: " <> str
formatParseError (UnExpect    str) = "Unexpected: " <> str
formatParseError (Expect      str) = "Expected: "   <> str
formatParseError (Message     str) = str

-------------------------------------------------------------------------------
-- Utilities

-- | Gather values.
mangle :: (Semigroup s, Monoid m) => (a -> s) -> (b -> m) -> [Either a b] -> Either s m
mangle toS toM xs = case (nonEmpty . lefts &&& rights) xs of
  (Just as, _)  -> let ss = toS <$> as in Left  (sconcat ss)
  (Nothing, bs) -> let ms = toM <$> bs in Right (mconcat ms)
