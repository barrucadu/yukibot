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
import Control.Monad (when, void)
import Control.Monad.Catch (SomeException, catch)
import Data.Either (lefts, rights)
import Data.Foldable (toList)
import qualified Data.HashMap.Strict as H
import Data.List (isPrefixOf)
import Data.List.NonEmpty (NonEmpty(..), fromList)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Monoid ((<>), mconcat)
import Data.Semigroup (Semigroup, sconcat)
import Data.Text (Text, unpack)
import qualified Data.Text as T
import System.Exit (die)
import System.FilePath (FilePath)
import System.Posix.Signals (Handler(..), installHandler, sigINT, sigTERM)

import Yukibot.Backend (startBackend, stopBackend, awaitStop)
import Yukibot.Configuration
import Yukibot.Types

-- | A 'BackendHandle' with the type parameters hidden.
data WrappedHandle where
  Wrap :: BackendHandle channel user -> WrappedHandle

-- | A default @main@ function: parse the config file, and either
-- halt+report errors, or start.
defaultMain :: BotState -> FilePath -> IO ()
defaultMain st fp = do
  mcfg <- parseConfigFile fp
  case mcfg of
    Right cfg -> case makeBot st cfg of
      Right go   -> go
      Left  errs -> die ("Error creating bot: " ++ show errs)
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

-- | Create a bot with the given state and configuration, this
-- terminates when all backends are stopped.
--
-- Returns @NoSuchBackend@ if any backends in the configuration are
-- not specified in the initial state. TODO: Is this a useful
-- behaviour? Should unknown backends be ignored instead?
makeBot :: BotState -> Table -> Either (NonEmpty CoreError) (IO ())
makeBot st cfg = case configuredBackends (getBackends st) (getPlugins st) cfg of
  Right bs -> Right $ do
    -- Start all backends
    hs <- mapM startWithPlugins bs
    -- Install signal handlers to kill backends
    void $ installHandler sigINT  (Catch $ mapM_ killBackend hs) Nothing
    void $ installHandler sigTERM (Catch $ mapM_ killBackend hs) Nothing
    -- Wait for termination
    mapM_ waitStop hs
  Left es -> Left es

  where
  -- Start a backend with the provided plugins.
  startWithPlugins :: (Backend, [Monitor]) -> IO WrappedHandle
  startWithPlugins (Backend b, enabledMonitors) = Wrap <$> startBackend handle b where
    -- Unlike in original yukibot, the monitors (for a single backend)
    -- are run in a single thread. This makes output more
    -- deterministic when multiple monitors fire on the same event,
    -- and in practice most events are only handled by one monitor, so
    -- this saves needless forking as well.
    handle ev = mapM_ (\(Monitor monitor) -> monitor ev) enabledMonitors

  -- Kill a backend
  killBackend :: WrappedHandle -> IO ()
  killBackend (Wrap h) = stopBackend h `catch` (\(_ :: SomeException) -> pure ())

  -- Wait for a backend to stop.
  waitStop :: WrappedHandle -> IO ()
  waitStop (Wrap h) = awaitStop h

-------------------------------------------------------------------------------
-- Configuration

-- | Configure and instantiate all backends.
configuredBackends :: H.HashMap BackendName (Text -> Table -> Either Text Backend)
  -- ^ The backends.
  -> H.HashMap PluginName (Table -> Either Text Plugin)
  -- ^ The plugins.
  -> Table
  -- ^ The global config.
  -> Either (NonEmpty CoreError) [(Backend, [Monitor])]
configuredBackends allBackends allPlugins cfg0 = mangle id (:[]) . concat $
  [ get (BackendName n) cfgs
  | (n, VTable cfgs) <- maybe [] H.toList (getTable "backend" cfg0)
  ]
  where
    -- Instantiate all backends of the given type from the config.
    get :: BackendName -> Table -> [Either (NonEmpty CoreError) (Backend, [Monitor])]
    get name cfgs = case H.lookup name allBackends of
      Just b -> flip concatMap (H.toList cfgs) $ \case
        (n, VTable  c)  -> [make b name n (c `override` cfg0)]
        (n, VTArray cs) -> [make b name n (c `override` cfg0) | c <- toList cs]
        (n, _) -> [make b name n cfg0]
      _ -> [Left (BackendUnknown name:|[])]

    -- Instantiate an individual backend.
    make :: (Text -> Table -> Either Text Backend)
         -> BackendName
         -> Text
         -> Table
         -> Either (NonEmpty CoreError) (Backend, [Monitor])
    make bf bname name cfg = case bf name cfg of
      Right (Backend b)  ->
        let enabledPlugins = configuredPlugins allPlugins bname name cfg
            -- Override the log files of the backend with values from
            -- the configuration, if present.
            b' = b { unrawLogFile = maybe (unrawLogFile b) unpack $ getString "logfile"    cfg
                   , rawLogFile   = maybe (rawLogFile   b) unpack $ getString "rawlogfile" cfg
                   }
        in (\ms -> (Backend b', ms)) <$> mangle id id enabledPlugins
      Left err -> Left (BackendBadConfig bname name err:|[])

-- | Configure and instantiate all plugins of a backend.
configuredPlugins :: H.HashMap PluginName (Table -> Either Text Plugin)
  -- ^ The plugins.
  -> BackendName
  -- ^ The name of the backend.
  -> Text
  -- ^ The name of this particular instance of the backend.
  -> Table
  -- ^ The configuration.
  -> [Either (NonEmpty CoreError) [Monitor]]
configuredPlugins allPlugins bname name cfg = [make (PluginName n) | n <- getStrings "plugins" cfg] where
  -- Instantiate a plugin.
  make :: PluginName -> Either (NonEmpty CoreError) [Monitor]
  make n = case H.lookup n allPlugins of
    Just toP -> case toP (pcfg n) of
      Right plugin -> mangle (:|[]) (:[]) (pluginToMonitors bname name n plugin cfg)
      Left err -> Left (PluginBadConfig bname name n err:|[])
    Nothing  -> Left (PluginUnknown bname name n:|[])

  -- Get the configuration of a plugin
  pcfg :: PluginName -> Table
  pcfg n = fromMaybe H.empty $ getNestedTable ["plugin", getPluginName n] cfg

-- | Convert a plugin into a list of monitors: one for every matching
-- monitor or command in the configuration.
pluginToMonitors :: BackendName
  -- ^ The name of the backend.
  -> Text
  -- ^ The name of this particular instance of the backend.
  -> PluginName
  -- ^ The name of the plugin.
  -> Plugin
  -- ^ The plugin.
  -> Table
  -- ^ The configuration.
  -> [Either CoreError Monitor]
pluginToMonitors bname name pname plugin cfg = map makeMonitor theMonitors ++ map makeCommandMonitor theCommands where
  -- Look up a monitor by name.
  makeMonitor :: MonitorName -> Either CoreError Monitor
  makeMonitor m = case H.lookup m (pluginMonitors plugin) of
    Just monitor -> Right monitor
    Nothing      -> Left (MonitorUnknown bname name pname m)

  -- Look up a command by name and turn it into a monitor.
  makeCommandMonitor :: (Text, CommandName) -> Either CoreError Monitor
  makeCommandMonitor (v, c) = case H.lookup c (pluginCommands plugin) of
    Just command -> Right (commandToMonitor v command)
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
commandToMonitor :: Text -> Command -> Monitor
commandToMonitor v (Command c) = Monitor $ \ev -> do
  let vs    = T.words v
  let parts = T.words (eventMessage ev)
  let args  = drop (length vs) parts

  when (vs `isPrefixOf` parts) (c ev args)
