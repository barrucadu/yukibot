{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Module      : Yukibot.Types
-- Copyright   : (c) 2016 Michael Walker
-- License     : MIT
-- Stability   : experimental
-- Portability : GADTs, RankNTypes
module Yukibot.Types
  ( -- * Events
    Event(..)
  -- * Actions
  , Action(..)
  -- * Backends
  , Backend(..)
  , BackendHandle(..)
  , BackendTerminatedException(..)
  -- * Logging
  , Logger(..)
  , Direction(..)
  -- * Plugins
  , Plugin(..)
  ) where

import Control.Concurrent.STM (TQueue, TVar)
import Control.Monad.Catch (Exception)
import Data.Text (Text)

-------------------------------------------------------------------------------
-- Events

data Event channel user = Event
  { eventHandle  :: BackendHandle channel user
  , eventChannel :: Maybe channel
  , eventUser    :: user
  , eventMessage :: Text
  }

-------------------------------------------------------------------------------
-- Actions

data Action channel user
  = Join channel
  -- ^ Join a new channel.
  | Leave channel
  -- ^ Leave a current channel.
  | Say channel [user] Text
  -- ^ Send a message to a channel, optionally addressed to a collection of users.
  | Whisper user Text
  -- ^ Send a message to a user.
  | Terminate
  -- ^ Gracefully disconnect.
  deriving (Eq, Read, Show)

-------------------------------------------------------------------------------
-- Backends

-- | A representation of a backend, it is parameterised by the channel
-- and user types.
--
-- TODO: Have configuration determine description.
--
-- TODO: Do raw logging in backend, event/action logging in core.
data Backend channel user where
  Backend :: { initialise :: ((BackendHandle channel user -> IO (Event channel user)) -> IO ()) -> IO a
             , run :: TQueue (Action channel user) -> a -> IO ()
             , describe :: Text
             } -> Backend channel user

-- | An abstract handle to a backend, which can be used to interact
-- with it.
data BackendHandle channel user = BackendHandle
  { msgQueue    :: TQueue (Action channel user)
  , hasStarted  :: TVar Bool
  , hasStopped  :: TVar Bool
  , description :: Text
  }
  deriving Eq

data BackendTerminatedException = BackendTerminatedException
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

instance Exception BackendTerminatedException

-------------------------------------------------------------------------------
-- Logging

data Logger channel user = Logger
  { loggerToServer   :: Text -> IO ()
  -- ^ Log a raw message sent to the server.
  , loggerFromServer :: Text -> IO ()
  -- ^ Log a raw message received from the server.
  , loggerEvent      :: Event channel user -> IO ()
    -- ^ Log an event received from the backend.
  , loggerAction     :: Action channel user -> IO ()
    -- ^ Log an action sent to the backend.
  }

data Direction = ToServer | FromServer
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

-------------------------------------------------------------------------------
-- Plugins

newtype Plugin = Plugin (forall channel user. Event channel user -> IO ())
