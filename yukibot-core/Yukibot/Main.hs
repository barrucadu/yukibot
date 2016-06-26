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
import Control.Monad (void)
import Control.Monad.Catch (SomeException, catch)
import Data.Either (lefts, rights)
import Data.Foldable (toList)
import qualified Data.HashMap.Strict as H
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text, unpack)
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
    Just cfg -> case makeBot st cfg of
      Right go   -> go
      Left  errs -> die ("Error creating bot: " ++ show errs)
    Nothing -> die "Error parsing configuration file."

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
  startWithPlugins :: (Backend, [Plugin]) -> IO WrappedHandle
  startWithPlugins (Backend b, enabledPlugins) = Wrap <$> startBackend handle b where
    -- Unlike in original yukibot, the plugins (for a single backend)
    -- are run in a single thread. This makes output more
    -- deterministic when multiple plugins fire on the same event, and
    -- in practice most plugins only deal with one type of event, so
    -- this saves needless forking as well.
    handle ev = mapM_ (\(Plugin plugin) -> plugin ev) enabledPlugins

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
  -> Either (NonEmpty CoreError) [(Backend, [Plugin])]
configuredBackends allBackends allPlugins cfg = mangle [ get (BackendName n) c
                                                       | (n, VTable c) <- maybe [] H.toList (getTable "backend" cfg)
                                                       ]
  where
    -- Gather the errors.
    mangle :: [[Either [CoreError] (Backend, [Plugin])]] -> Either (NonEmpty CoreError) [(Backend, [Plugin])]
    mangle xs = case (concat . lefts &&& rights) (concat xs) of
      ([], bs) -> Right bs
      (e:es, _) -> Left (e:|es)

    -- Instantiate all backends of the given type from the config.
    get :: BackendName -> Table -> [Either [CoreError] (Backend, [Plugin])]
    get name tbl = case H.lookup name allBackends of
      Just b -> flip concatMap (H.toList tbl) $ \case
        (n, VTable  c)  -> [make b name n c]
        (n, VTArray cs) -> [make b name n c | c <- toList cs]
        _ -> []
      _ -> [Left [BackendUnknown name]]

    -- Instantiate an individual backend.
    make :: (Text -> Table -> Either Text Backend) -> BackendName -> Text -> Table -> Either [CoreError] (Backend, [Plugin])
    make bf bname name tbl = case bf name tbl of
      Right (Backend b)  ->
        let enabledPlugins = configuredPlugins allPlugins bname name cfg tbl
            -- Override the log files of the backend with values from
            -- the configuration, if present.
            b' = b { unrawLogFile = maybe (unrawLogFile b) unpack $ getString "logfile"    tbl
                   , rawLogFile   = maybe (rawLogFile   b) unpack $ getString "rawlogfile" tbl
                   }
        in case (lefts &&& rights) enabledPlugins of
          ([], ps) -> Right (Backend b', ps)
          (es, _)  -> Left es
      Left err -> Left [BackendBadConfig bname name err]

-- | Configure and instantiate all plugins of a backend.
configuredPlugins :: H.HashMap PluginName (Table -> Either Text Plugin)
  -- ^ The plugins.
  -> BackendName
  -- ^ The name of the backend.
  -> Text
  -- ^ The name of this particular instance of the backend.
  -> Table
  -- ^ The global configuration.
  -> Table
  -- ^ The backend configuration.
  -> [Either CoreError Plugin]
configuredPlugins allPlugins bname name cfg inst = case (getStrings "plugins" inst, getTable "plugins" inst) of
  -- Plugins with backend-specific config
  (_, Just plugins) -> [make' (PluginName n) c | (n, c) <- H.toList plugins]
  -- No backend-specific config
  (plugins, _) -> [make (PluginName n) H.empty | n <- plugins]

  where
    -- Instantiate a plugin with some config.
    make :: PluginName -> Table -> Either CoreError Plugin
    make n c = case H.lookup n allPlugins of
      Just toP -> either (Left . PluginBadConfig bname name n) Right (toP (pcfg n c))
      Nothing  -> Left (PluginUnknown bname name n)

    -- Instantiate a plugin with some config.
    make' :: PluginName -> Node -> Either CoreError Plugin
    make' n (VTable c) = make n c
    make' n _ = make n H.empty

    -- Look up the global config and merge with the backend-specific
    -- config, with backend-specific taking priority.
    pcfg :: PluginName -> Table -> Table
    pcfg (PluginName n) c = c <> fromMaybe H.empty (getNestedTable ["plugin", n] cfg)

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
