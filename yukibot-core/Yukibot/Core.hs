{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Yukibot.Core
-- Copyright   : (c) 2016 Michael Walker
-- License     : MIT
-- Stability   : experimental
-- Portability : GADTs, LambdaCase, OverloadedStrings, ScopedTypeVariables
module Yukibot.Core
    ( -- * Execution
      defaultMain
    , makeBot

      -- * State
    , BotState(..)
    , initialBotState
      -- ** Backends
    , WrappedBackend(WrapB)
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
import Data.Text (Text, unpack)
import System.Exit (die)
import System.FilePath (FilePath)
import System.Posix.Signals (Handler(..), installHandler, sigINT, sigTERM)

import Yukibot.Backend (startBackend, stopBackend, awaitStop)
import Yukibot.Configuration
import Yukibot.Types

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
makeBot st cfg = case (lefts &&& rights) configuredBackends of
  ([], bs) -> Right $ do
    -- Start all backends
    hs <- mapM startWithPlugins bs
    -- Install signal handlers to kill backends
    void $ installHandler sigINT  (Catch $ mapM_ killBackend hs) Nothing
    void $ installHandler sigTERM (Catch $ mapM_ killBackend hs) Nothing
    -- Wait for termination
    mapM_ waitStop hs
  (e:es, _) -> Left (e:|es)

  where
  -- Get all backends from the config.
  configuredBackends :: [Either CoreError (WrappedBackend, [Text])]
  configuredBackends = maybe [] (concatMap get . H.toList) (getTable "backend" cfg) where
    get (name, VTable tbl) = case H.lookup name (backends st) of
      Just wrapped@(WrapB _) -> flip concatMap (H.toList tbl) $ \case
        (n, VTable  c)  -> [make wrapped n c]
        (n, VTArray cs) -> [make wrapped n c | c <- toList cs]
        _ -> []
      _ -> [Left $ BackendUnknown name]
    get _ = []
    make (WrapB wrapped) name inst = case wrapped name inst of
      Right b  ->
        let enabledPlugins = getStrings "plugins" inst
            -- Override the log files of the backend with values from
            -- the configuration, if present.
            b' = b { unrawLogFile = maybe (unrawLogFile b) unpack $ getString "logfile"    inst
                   , rawLogFile   = maybe (rawLogFile   b) unpack $ getString "rawlogfile" inst
                   }
        in Right (WrapBC b', enabledPlugins)
      Left err -> Left (BackendBadConfig name err)
    make _ _ _ = error "makeBot.configuredBackends: 'impossible' state!"

  -- Start a backend with the provided plugins.
  startWithPlugins (WrapBC b, eplugins) = WrapBH <$> startBackend handle b where
    -- Unlike in original yukibot, the plugins (for a single backend)
    -- are run in a single thread. This makes output more
    -- deterministic when multiple plugins fire on the same event, and
    -- in practice most plugins only deal with one type of event, so
    -- this saves needless forking as well.
    handle ev = mapM_ (\(Plugin plugin) -> plugin ev) enabledPlugins
    enabledPlugins = H.filterWithKey (\k _ -> k `elem` eplugins) (plugins st)
  startWithPlugins _ = error "makeBot.startWithPlugins: 'impossible' state!"

  -- Kill a backend
  killBackend (WrapBH h) = stopBackend h `catch` (\(_ :: SomeException) -> pure ())
  killBackend _ = pure () -- should never happen

  -- Wait for a backend to stop.
  waitStop (WrapBH h) = awaitStop h
  waitStop _ = pure () -- should never happen

-------------------------------------------------------------------------------
-- State

-- | Initial state for a bot.
data BotState = BotState
  { backends :: H.HashMap Text WrappedBackend
  , plugins  :: H.HashMap Text Plugin
  }

initialBotState :: BotState
initialBotState = BotState { backends = H.empty
                           , plugins  = H.empty
                           }

-- | A 'Backend' with the type parameters hidden.
data WrappedBackend where
  WrapB  :: (Text -> Table -> Either Text (Backend channel user)) -> WrappedBackend
  WrapBC :: Backend channel user -> WrappedBackend
  WrapBH :: BackendHandle channel user -> WrappedBackend

-- | Add a new backend.
--
-- Returns @BackendNameClash@ if there is a clash.
addBackend :: Text
  -- ^ The name of the backend (e.g. \"irc\")
  -> (Text -> Table -> Either Text (Backend channel user))
  -- ^ The instantiation function.
  -> BotState -> Either CoreError BotState
addBackend name backend st
  | H.member name (backends st) = Left (BackendNameClash name)
  | otherwise = Right st { backends = H.insert name (WrapB backend) (backends st) }

-- | Add a new plugin.
--
-- Returns @PluginNameClash@ if there is a clash.
addPlugin :: Text
  -- ^ The name of the plugin (e.g. \"hello\")
  -> Plugin
  -- ^ The plugin
  -> BotState -> Either CoreError BotState
addPlugin name plugin st
  | H.member name (plugins st) = Left (PluginNameClash name)
  | otherwise = Right st { plugins = H.insert name plugin (plugins st) }

-------------------------------------------------------------------------------
-- Errors

-- | An error in the core.
data CoreError
  = BackendNameClash !Text
  -- ^ A backend was added where the name was already taken.
  | BackendUnknown !Text
  -- ^ A backend was requested but it is unknown.
  | BackendBadConfig !Text !Text
  -- ^ The configuration for a backend is invalid.
  | PluginNameClash !Text
  -- ^ A plugin was added where the name was already taken.
  deriving (Eq, Ord, Read, Show)
