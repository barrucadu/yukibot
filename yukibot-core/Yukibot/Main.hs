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
    , configuredBackends
    , configuredPlugins
    , pluginToMonitors

      -- * State
    , BotState
    , emptyBotState
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

import Yukibot.Backend (startBackend, stopBackend, awaitStop)
import Yukibot.Configuration
import Yukibot.Plugin.Builtin
import Yukibot.Types

-- | A default @main@ function: set up the \"builtin\" plugin, parse
-- the config file, and either halt+report errors, or start.
defaultMain :: BotState -> FilePath -> IO ()
defaultMain st0 fp = do
  mcfg <- parseConfigFile fp
  case mcfg of
    Right cfg -> do
      state <- initialBuiltinState cfg
      let getUser0   = builtinGetUser   state
      let getPrefix0 = builtinGetPrefix state
      let setUser0   = builtinSetUser   state
      let isMonitorEnabled = builtinIsMonitorEnabled state
      let isCommandEnabled = builtinIsCommandEnabled state
      case addPlugin "builtin" (builtinPlugin state) st0 of
        Right st -> case makeBot st setUser0 getUser0 getPrefix0 isMonitorEnabled isCommandEnabled cfg of
          Right go   -> go
          Left  errs -> die ("En error while creating the bot: " ++ show errs)
        Left err -> die ("An error occurred while adding the \"builtin\" plugin: " ++ show err)
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
-- action terminates when all backends are stopped.
makeBot :: BotState
  -- ^ The initial state. This should include the \"builtin\" plugin
  -- (or at least, one with that name).
  -> (BackendName -> Text -> Int -> UserName -> IO ())
  -- ^ Callback for when the username changes.
  -> (BackendName -> Text -> Int -> IO UserName)
  -- ^ Get the real username.
  -> (BackendName -> Text -> Int -> Maybe ChannelName -> IO Text)
  -- ^ Get the command prefix.
  -> (BackendName -> Text -> Int -> ChannelName -> PluginName -> MonitorName -> IO Bool)
  -- ^ Check if a monitor is enabled.
  -> (BackendName -> Text -> Int -> ChannelName -> PluginName -> CommandName -> IO Bool)
  -- ^ Check if a command is enabled.
  -> Table
  -- ^ The global configuration.
  -> Either (NonEmpty CoreError) (IO ())
makeBot st setUser0 getUser0 getPrefix0 isMonitorEnabled isCommandEnabled cfg =
  case configuredBackends (getBackends st) (getPlugins st) getUser0 getPrefix0 cfg of
    Right bs -> Right $ do
      -- Start all backends
      hs <- mapM startWithPlugins bs
      -- Install signal handlers to kill backends
      void $ installHandler sigINT  (Catch $ mapM_ killBackend hs) Nothing
      void $ installHandler sigTERM (Catch $ mapM_ killBackend hs) Nothing
      -- Wait for termination
      mapM_ awaitStop hs
    Left es -> Left es

  where
  -- Start a backend with the provided plugins.
  startWithPlugins (bname, sname, index, b, enabledMonitors) =
    startBackend handle (setUser0 bname sname index) bname sname index b
    where
      -- Unlike in original yukibot, the monitors (for a single
      -- backend) are run in a single thread. This makes output more
      -- deterministic when multiple monitors fire on the same event,
      -- and in practice most events are only handled by one monitor,
      -- so this saves needless forking as well.
      handle ev = for_ enabledMonitors $ \case
        (pn, Left mn, Monitor monitor) -> case eventChannel ev of
          Just cname -> isMonitorEnabled bname sname index cname pn mn >>= flip when (monitor ev)
          Nothing    -> monitor ev
        (pn, Right cn, Monitor monitor) -> case eventChannel ev of
          Just cname -> isCommandEnabled bname sname index cname pn cn >>= flip when (monitor ev)
          Nothing    -> monitor ev

  -- Kill a backend
  killBackend h = stopBackend h `catch` (\(_ :: SomeException) -> pure ())

-------------------------------------------------------------------------------
-- Configuration

-- | Configure and instantiate all backends.
configuredBackends :: H.HashMap BackendName (Text -> Table -> Either Text Backend)
  -- ^ The backends.
  -> H.HashMap PluginName (Table -> Either Text Plugin)
  -- ^ The plugins. This should include the \"builtin\" plugin (or at
  -- least, one with that name).
  -> (BackendName -> Text -> Int -> IO UserName)
  -- ^ Get the real username.
  -> (BackendName -> Text -> Int -> Maybe ChannelName -> IO Text)
  -- ^ Get the command prefix.
  -> Table
  -- ^ The global config.
  -> Either (NonEmpty CoreError) [(BackendName, Text, Int, Backend, [(PluginName, Either MonitorName CommandName, Monitor)])]
configuredBackends allBackends allPlugins getUser0 getPrefix0 cfg0 = mangle id (:[]) . concat $
  [ get (BackendName n) cfgs
  | (n, VTable cfgs) <- maybe [] H.toList (getTable "backend" cfg0)
  ]
  where
    -- Instantiate all backends of the given type from the config.
    get :: BackendName
        -> Table
        -> [Either (NonEmpty CoreError) (BackendName, Text, Int, Backend, [(PluginName, Either MonitorName CommandName, Monitor)])]
    get name cfgs = case H.lookup name allBackends of
      Just b -> flip concatMap (H.toList cfgs) $ \case
        (n, VTable  c)  -> [make b name n 0 (c `override` cfg0)]
        (n, VTArray cs) -> [make b name n i (c `override` cfg0) | (i, c) <- zip [0..] (toList cs)]
        (n, _) -> [make b name n 0 cfg0]
      _ -> [Left (BackendUnknown name:|[])]

    -- Instantiate an individual backend.
    make :: (Text -> Table -> Either Text Backend)
         -> BackendName
         -> Text
         -> Int
         -> Table
         -> Either (NonEmpty CoreError) (BackendName, Text, Int, Backend, [(PluginName, Either MonitorName CommandName, Monitor)])
    make bf bname sname index cfg = case bf sname cfg of
      Right b  ->
        let getUser   = getUser0   bname sname index
            getPrefix = getPrefix0 bname sname index
            enabledPlugins = configuredPlugins allPlugins getUser getPrefix bname sname cfg
            -- Override the log files of the backend with values from
            -- the configuration, if present.
            b' = b { unrawLogFile = maybe (unrawLogFile b) unpack $ getString "logfile"    cfg
                   , rawLogFile   = maybe (rawLogFile   b) unpack $ getString "rawlogfile" cfg
                   }
        in (\ms -> (bname, sname, index, b', ms)) <$> mangle id id enabledPlugins
      Left err -> Left (BackendBadConfig bname sname err:|[])

-- | Configure and instantiate all plugins of a backend.
--
-- The \"builtin\" plugin is automatically added to the enabled list,
-- but NOT automatically inserted into the plugin map, as it needs
-- some special set-up. This means that if you call this function
-- WITHOUT adding \"builtin\" to the map you WILL get an unknown
-- plugin error!
configuredPlugins :: H.HashMap PluginName (Table -> Either Text Plugin)
  -- ^ The plugins.
  -> IO UserName
  -- ^ Get the username.
  -> (Maybe ChannelName -> IO Text)
  -- ^ Get the command prefix.
  -> BackendName
  -- ^ The name of the backend.
  -> Text
  -- ^ The name of this particular instance of the backend.
  -> Table
  -- ^ The configuration.
  -> [Either (NonEmpty CoreError) [(PluginName, Either MonitorName CommandName, Monitor)]]
configuredPlugins allPlugins getUser getPrefix bname name cfg =
  [make (PluginName n) | n <- nub $ "builtin":getStrings "plugins" cfg]
  where
    -- Instantiate a plugin.
    make :: PluginName -> Either (NonEmpty CoreError) [(PluginName, Either MonitorName CommandName, Monitor)]
    make n = case H.lookup n allPlugins of
      Just toP -> case toP (pcfg n) of
        Right plugin -> case mangle (:|[]) (:[]) (pluginToMonitors getUser getPrefix bname name n plugin cfg)  of
          Right ms -> Right [(n, e, m) | (e, m) <- ms]
          Left  es -> Left es
        Left err -> Left (PluginBadConfig bname name n err:|[])
      Nothing  -> Left (PluginUnknown bname name n:|[])

    -- Get the configuration of a plugin
    pcfg :: PluginName -> Table
    pcfg n = fromMaybe H.empty $ getNestedTable ["plugin", getPluginName n] cfg

-- | Convert a plugin into a list of monitors: one for every matching
-- monitor or command in the configuration.
pluginToMonitors :: IO UserName
  -- ^ Get the username.
  -> (Maybe ChannelName -> IO Text)
  -- ^ Get the command prefix.
  -> BackendName
  -- ^ The name of the backend.
  -> Text
  -- ^ The name of this particular instance of the backend.
  -> PluginName
  -- ^ The name of the plugin.
  -> Plugin
  -- ^ The plugin.
  -> Table
  -- ^ The configuration.
  -> [Either CoreError (Either MonitorName CommandName, Monitor)]
pluginToMonitors getUser getPrefix bname name pname plugin cfg =
  map makeMonitor theMonitors ++ map makeCommandMonitor theCommands
  where
    -- Look up a monitor by name.
    makeMonitor :: MonitorName -> Either CoreError (Either MonitorName CommandName, Monitor)
    makeMonitor m = case H.lookup m (pluginMonitors plugin) of
      Just monitor -> Right (Left m, monitor)
      Nothing      -> Left (MonitorUnknown bname name pname m)

    -- Look up a command by name and turn it into a monitor.
    makeCommandMonitor :: (Text, CommandName) -> Either CoreError (Either MonitorName CommandName, Monitor)
    makeCommandMonitor (v, c) = case H.lookup c (pluginCommands plugin) of
      Just command -> Right (Right c, commandToMonitor getUser getPrefix v command)
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
getBackends :: BotState
  -> H.HashMap BackendName (Text -> Table -> Either Text Backend)
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
getPlugins :: BotState
  -> H.HashMap PluginName (Table -> Either Text Plugin)
getPlugins = stPlugins

-------------------------------------------------------------------------------
-- Utilities

-- | Gather values.
mangle :: (Semigroup s, Monoid m) => (a -> s) -> (b -> m) -> [Either a b] -> Either s m
mangle toS toM xs = case (lefts &&& rights) xs of
  ([], bs) -> let ms = map toM bs in Right (mconcat ms)
  (as, _)  -> let ss = map toS as in Left  (sconcat $ fromList ss)

-- | Given a verb, convert a 'Command' to a 'Monitor'.
commandToMonitor :: IO UserName
  -- ^ Get the username.
  -> (Maybe ChannelName -> IO Text)
  -- ^ Get the channel prefix.
  -> Text
  -- ^ The verb.
  -> Command -> Monitor
commandToMonitor getUser getPrefix v (Command cmd) = Monitor $ \ev@(Event _ c _ m) -> do
  prefixes <- getPrefixes c
  case check (isNothing c) prefixes m of
    Just args -> cmd ev args
    Nothing   -> pure ()

  where
    -- Get the allowed prefixes
    getPrefixes c = do
      UserName user <- getUser
      prefix <- getPrefix c
      pure [prefix, user <> ": ", user <> ", ", user <> " "]

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
