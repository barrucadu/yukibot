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
  , RawLogger(..)
  -- * Plugins
  , Plugin(..)
  ) where

import Control.Concurrent.STM (TQueue, TVar)
import Control.Monad.Catch (Exception)
import Data.ByteString (ByteString)
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
  Backend :: { initialise :: RawLogger -> ((BackendHandle channel user -> Event channel user) -> IO ()) -> IO a
             , run :: TQueue (Action channel user) -> a -> IO ()
             , describe :: Text
             , showChannel :: channel -> Text
             , showUser :: user -> Text
             , rawLogFile :: FilePath
             , unrawLogFile :: FilePath
             } -> Backend channel user

-- | An abstract handle to a backend, which can be used to interact
-- with it.
data BackendHandle channel user = BackendHandle
  { msgQueue     :: TQueue (Action channel user)
  , hasStarted   :: TVar Bool
  , hasStopped   :: TVar Bool
  , description  :: Text
  , actionLogger :: Action channel user -> IO ()
  }

instance Eq (BackendHandle channel user) where
  h1 == h2 = msgQueue h1 == msgQueue h2

data BackendTerminatedException = BackendTerminatedException
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

instance Exception BackendTerminatedException

-------------------------------------------------------------------------------
-- Logging

-- | A logger of events and actions received from and sent to the
-- backend by the bot.
data Logger channel user = Logger
  { loggerEvent :: Event channel user -> IO ()
    -- ^ Log an event received from the backend.
  , loggerAction :: Action channel user -> IO ()
    -- ^ Log an action sent to the backend.
  }

-- | A logger of raw messages sent to and from the server by the
-- backend.
data RawLogger = RawLogger
  { rawToServer :: ByteString -> IO ()
  -- ^ Log a raw message sent to the server.
  , rawFromServer :: ByteString -> IO ()
  -- ^ Log a raw message received from the server.
  }

-------------------------------------------------------------------------------
-- Plugins

newtype Plugin = Plugin (forall channel user. Event channel user -> IO ())
