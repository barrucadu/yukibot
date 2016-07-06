{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Yukibot.Main
-- Copyright   : (c) 2016 Michael Walker
-- License     : MIT
-- Stability   : experimental
-- Portability : GADTs, LambdaCase, OverloadedStrings, ScopedTypeVariables
module Yukibot.Main
    ( -- * Execution
      defaultMain
    , makeBot

    -- * Configuration
    , instantiateBackends
    , instantiateBackend
    , instantiatePlugins
    , instantiatePlugin

      -- * State
    , BotState
    , BuiltinState
    , emptyBotState
    , initialBuiltinState
      -- ** Backends
    , addBackend
    -- ** Plugins
    , addPlugin

    -- * Errors
    , CoreError(..)
    ) where

import Control.Arrow ((&&&))
import Control.Monad (guard, when, void)
import Control.Monad.Catch (SomeException, catch)
import Data.Either (lefts, rights)
import Data.Foldable (for_, toList)
import qualified Data.HashMap.Strict as H
import Data.List (isPrefixOf, nub)
import Data.List.NonEmpty (NonEmpty(..), fromList)
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
import Yukibot.Plugin.Builtin
import Yukibot.Types

-- | A default @main@ function: parse the config file and either
-- halt+report errors, or start.
defaultMain :: BotState -> FilePath -> IO ()
defaultMain st0 fp = do
  mcfg <- parseConfigFile fp
  case mcfg of
    Right cfg -> do
      builtinst <- initialBuiltinState cfg
      case makeBot st0 builtinst cfg of
        Right go   -> go
        Left  errs -> die ("En error while creating the bot: " ++ show errs)
    Left err ->
      let pos     = errorPos err
          line    = show (sourceLine   pos)
          col     = show (sourceColumn pos)
          msgs    = errorMessages err
          msgs'   = if null msgs then ["Unknown error."] else map formatMsg msgs
          msgList = mconcat . map ("\n  • "<>) $ msgs'
      in die ("An error occurred while parsing the configuration file at line " <> line <> ":, column " <> col <> ":" <> msgList)

  where
    -- Pretty-print a parse error.
    formatMsg :: Message -> String
    formatMsg (SysUnExpect str) = "Unexpected: " <> str
    formatMsg (UnExpect    str) = "Unexpected: " <> str
    formatMsg (Expect      str) = "Expected: "   <> str
    formatMsg (Message     str) = str

-- | Create a bot with the given state and configuration, the returned
-- action terminates when all backends are stopped. This automatically
-- sets up the \"builtin\" plugin.
makeBot :: BotState
  -- ^ The initial state of the bot.
  -> BuiltinState
  -- ^ The initial state of the \"builtin\" plugin. This is normally
  -- supplied by 'defaultMain'.
  -> Table
  -- ^ The global configuration.
  -> Either (NonEmpty CoreError) (IO ())
makeBot st0 builtinst cfg = do
  -- Add the \"builtin\" plugin
  st <- either (Left . (:|[])) Right $
        addPlugin "builtin" (builtinPlugin builtinst) st0
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
      -- Unlike in original yukibot, the monitors (for a single
      -- backend) are run in a single thread. This makes output more
      -- deterministic when multiple monitors fire on the same event,
      -- and in practice most events are only handled by one monitor,
      -- so this saves needless forking as well.
      handle ev = for_ enabledMonitors $ \(pn, mnOrCn, Monitor monitor) -> do
        enabled <- isEnabled pn (eventChannel ev) mnOrCn
        when enabled . runBackendM builtinst ib ev $ monitor ev

      -- All monitors enabled in the backend.
      enabledMonitors = [ (instPluginName ip, mnOrCn, mon)
                        | ip <- instPlugins ib
                        , (mnOrCn, mon) <- instMonitors ip
                        ]

      -- Check if a monitor or command is enabled in a channel.
      isEnabled pn (Just cname) (Right cn) = isCommandEnabled' cname pn cn
      isEnabled pn (Just cname) (Left  mn) = isMonitorEnabled' cname pn mn
      isEnabled _ Nothing _ = pure True

      isCommandEnabled' = isCommandEnabled (instBackendName ib) (instSpecificName ib) (instIndex ib)
      isMonitorEnabled' = isMonitorEnabled (instBackendName ib) (instSpecificName ib) (instIndex ib)

    -- Kill a backend
    killBackend h = stopBackend h `catch` (\(_ :: SomeException) -> pure ())

    -- Check if a monitor is enabled in a channel.
    isMonitorEnabled = builtinIsMonitorEnabled builtinst

    -- Check if a command is enabled in a channel.
    isCommandEnabled = builtinIsCommandEnabled builtinst

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
        -- Override the log files of the backend with values from the
        -- configuration, if present.
        b' = b { unrawLogFile = maybe (unrawLogFile b) unpack $ getString "logfile"    cfg
               , rawLogFile   = maybe (rawLogFile   b) unpack $ getString "rawlogfile" cfg
               }
    in InstantiatedBackend bname sname index b' <$> mangle id (:[]) enabledPlugins
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
  -> [Either (NonEmpty CoreError) InstantiatedPlugin]
instantiatePlugins allPlugins bname name cfg =
  [make (PluginName n) | n <- nub $ "builtin":getStrings "plugins" cfg]
  where
    -- Instantiate a plugin.
    make :: PluginName -> Either (NonEmpty CoreError) InstantiatedPlugin
    make n = case H.lookup n allPlugins of
      Just toP -> case toP (pcfg n) of
        Right plugin -> instantiatePlugin bname name n plugin cfg
        Left err -> Left (PluginBadConfig bname name n err:|[])
      Nothing  -> Left (PluginUnknown bname name n:|[])

    -- Get the configuration of a plugin
    pcfg :: PluginName -> Table
    pcfg n = fromMaybe H.empty $ getNestedTable ["plugin", getPluginName n] cfg

-- | Instantiate an individual plugin.
instantiatePlugin :: BackendName
  -- ^ The name of the backend.
  -> Text
  -- ^ The name of this particular instance of the backend.
  -> PluginName
  -- ^ The name of the plugin.
  -> Plugin
  -- ^ The plugin.
  -> Table
  -- ^ The configuration.
  -> Either (NonEmpty CoreError) InstantiatedPlugin
instantiatePlugin bname name pname plugin cfg = case (lefts &&& rights) components of
  ([], mons) -> Right (InstantiatedPlugin pname mons)
  (e:es, _)  -> Left (e:|es)

  where
    components :: [Either CoreError (Either MonitorName CommandName, Monitor)]
    components = map makeMonitor theMonitors ++ map makeCommandMonitor theCommands

    -- Look up a monitor by name.
    makeMonitor :: MonitorName -> Either CoreError (Either MonitorName r, Monitor)
    makeMonitor m = case H.lookup m (pluginMonitors plugin) of
      Just monitor -> Right (Left m, monitor)
      Nothing      -> Left (MonitorUnknown bname name pname m)

    -- Look up a command by name and turn it into a monitor.
    makeCommandMonitor :: (Text, CommandName) -> Either CoreError (Either l CommandName, Monitor)
    makeCommandMonitor (v, c) = case H.lookup c (pluginCommands plugin) of
      Just command -> Right (Right c, commandToMonitor v command)
      Nothing      -> Left (CommandUnknown bname name pname c)

    -- All enabled monitors for this plugin/backend.
    theMonitors :: [MonitorName]
    theMonitors = mapMaybe restrict $ getStrings "monitors" cfg where
      restrict m = case T.breakOn ":" m of
        (pn, mn) | pn == getPluginName pname -> Just (MonitorName $ T.tail mn)
                 | otherwise -> Nothing

    -- All enabled commands for this plugin/backend.
    theCommands :: [(Text, CommandName)]
    theCommands = maybe [] (mapMaybe restrict . H.toList) $ getTable "commands" cfg where
      restrict (v, VString c) = case T.breakOn ":" c of
        (pn, cn) | pn == getPluginName pname -> Just (v, CommandName $ T.tail cn)
                 | otherwise -> Nothing
      restrict _ = Nothing

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
-- Utilities

-- | Gather values.
mangle :: (Semigroup s, Monoid m) => (a -> s) -> (b -> m) -> [Either a b] -> Either s m
mangle toS toM xs = case (lefts &&& rights) xs of
  ([], bs) -> let ms = map toM bs in Right (mconcat ms)
  (as, _)  -> let ss = map toS as in Left  (sconcat $ fromList ss)

-- | Given a verb, convert a 'Command' to a 'Monitor'.
commandToMonitor :: Text
  -- ^ The verb.
  -> Command -> Monitor
commandToMonitor v (Command cmd) = Monitor $ \ev@(Event _ _ c _ m) -> do
  prefix <- getCommandPrefix c
  let UserName user = eventWhoAmI ev
  let prefixes = [prefix, user <> ": ", user <> ", ", user <> " "]
  case check (isNothing c) prefixes m of
    Just args -> cmd ev args
    Nothing   -> pure ()

  where
    -- Check if any of the prefixes match the message, and return the
    -- args if so.
    check allowEmpty [] m
      | allowEmpty = stripVerb m
      | otherwise = Nothing
    check allowEmpty (p:ps) m = case stripVerb =<< T.stripPrefix p m of
      Just args -> Just args
      Nothing   -> check allowEmpty ps m

    -- Strip the verb from the start of a message.
    stripVerb m =
      let verbParts = T.words v
          msgParts  = T.words m
          args = drop (length verbParts) msgParts
      in guard (verbParts `isPrefixOf` msgParts) >> pure args
