{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Yukibot.Core
-- Copyright   : (2) 2016 Michael Walker
-- License     : MIT
-- Stability   : experimental
-- Portability : GADTs, LambdaCase, OverloadedStrings, ScopedTypeVariables
module Yukibot.Core
    ( -- * Execution
      makeBot

      -- * State
    , BotState(..)
    , initialBotState
      -- ** Backends
    , WrappedBackend(WrapB)
    , addBackend

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
import Data.Text (Text)
import System.Posix.Signals (Handler(..), installHandler, sigINT, sigTERM)

import Yukibot.Backend
import Yukibot.Configuration

-- | Create a bot with the given state and configuration, this
-- terminates when all backends are stopped.
--
-- Returns @NoSuchBackend@ if any backends in the configuration are
-- not specified in the initial state. TODO: Is this a useful
-- behaviour? Should unknown backends be ignored instead?
--
-- TODO: Plugins
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
  configuredBackends :: [Either CoreError WrappedBackend]
  configuredBackends = maybe [] (concatMap get . H.toList) (getTable "backend" cfg) where
    get (name, VTable tbl) = case H.lookup name (backends st) of
      Just wrapped@(WrapB _) -> flip concatMap (H.toList tbl) $ \case
        (n, VTable  c)  -> [make wrapped n c]
        (n, VTArray cs) -> [make wrapped n c | c <- toList cs]
        _ -> []
      _ -> [Left $ BackendUnknown name]
    get _ = []
    make (WrapB wrapped) name inst =
      either (Left . BackendBadConfig name) (Right . WrapBC) (wrapped name inst)
    make _ _ _ = error "makeBot.configuredBackends: 'impossible' state!"

  -- Start a backend with the provided plugins.
  startWithPlugins (WrapBC b) = WrapBH <$> startBackend handleEv b where
    handleEv _ = pure () -- todo
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
  { backends :: H.HashMap Text WrappedBackend }

initialBotState :: BotState
initialBotState = BotState { backends = H.empty }

-- | A 'Backend' with the type parameters hidden.
data WrappedBackend where
  WrapB  :: (Text -> Table -> Either Text (Backend channel user)) -> WrappedBackend
  WrapBC :: Backend channel user -> WrappedBackend
  WrapBH :: BackendHandle channel user -> WrappedBackend

-- | Add a new backend.
--
-- Returns @DuplicateBackendName@ if there is a clash.
addBackend :: Text
  -- ^ The name of the backend (e.g. \"irc\")
  -> (Text -> Table -> Either Text (Backend channel user))
  -- ^ The instantiation function.
  -> BotState -> Either CoreError BotState
addBackend name backend s = case H.lookup name (backends s) of
  Just _ -> Left BackendNameClash
  Nothing -> Right s { backends = H.insert name (WrapB backend) (backends s) }

-------------------------------------------------------------------------------
-- Errors

-- | An error in the core.
data CoreError
  = BackendNameClash
  -- ^ A backend was added where the name was already taken.
  | BackendUnknown !Text
  -- ^ A backend was requested but it is unknown.
  | BackendBadConfig !Text !Text
  -- ^ The configuration for a backend is invalid.
  deriving (Eq, Ord, Read, Show)
