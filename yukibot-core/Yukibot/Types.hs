{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Module      : Yukibot.Types
-- Copyright   : (c) 2016 Michael Walker
-- License     : MIT
-- Stability   : experimental
-- Portability : GADTs, GeneralizedNewtypeDeriving, RankNTypes
module Yukibot.Types
  ( -- * Events
    Event(..)
  -- * Actions
  , Action(..)
  -- * Backends
  , BackendName(..)
  , Backend(..)
  , Backend'(..)
  , BackendHandle(..)
  , BackendTerminatedException(..)
  -- * Logging
  , Logger(..)
  , RawLogger(..)
  , Tag(..)
  -- * Plugins
  , PluginName(..)
  , Plugin(..)
  , MonitorName(..)
  , Monitor(..)
  , CommandName(..)
  , Command(..)
  -- * Errors
  , CoreError(..)
  ) where

import Control.Concurrent.STM (TQueue, TVar)
import Control.Monad.Catch (Exception)
import Data.ByteString (ByteString)
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import Data.String (IsString)
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

newtype BackendName = BackendName { getBackendName :: Text }
  deriving (Eq, Ord, Read, Show, Hashable, IsString)

data Backend where
  Backend :: Backend' channel user -> Backend

-- | A representation of a backend, it is parameterised by the channel
-- and user types.
data Backend' channel user where
  Backend' :: { initialise :: RawLogger -> ((BackendHandle channel user -> Event channel user) -> IO ()) -> IO a
             , run :: TQueue (Action channel user) -> a -> IO ()
             , describe :: Text
             , showChannel :: channel -> Text
             , showUser :: user -> Text
             , rawLogFile :: FilePath
             , unrawLogFile :: FilePath
             } -> Backend' channel user

-- | A handle to a backend, which can be used to interact with it.
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

-- | A tag displayed in the stdout log to indicate which backend a
-- message is from. Tags are not included in the backend-specific log
-- files.
newtype Tag = Tag { getTag :: Text }
  deriving (Eq, Ord, Read, Show, Hashable, IsString)

-------------------------------------------------------------------------------
-- Plugins

newtype PluginName = PluginName { getPluginName :: Text }
  deriving (Eq, Ord, Read, Show, Hashable, IsString)

newtype MonitorName = MonitorName { getMonitorName :: Text }
  deriving (Eq, Ord, Read, Show, Hashable, IsString)

newtype CommandName = CommandName { getCommandName :: Text }
  deriving (Eq, Ord, Read, Show, Hashable, IsString)

-- | A plugin provides a collection of named monitors and commands.
data Plugin = Plugin
  { pluginMonitors :: HashMap MonitorName Monitor
  , pluginCommands :: HashMap CommandName Command
  }

-- | Monitors are activated on every message. They can communicate
-- with the backend using the supplied 'Event'.
newtype Monitor = Monitor (forall channel user. Event channel user -> IO ())

-- | Commands are like monitors, but they have a \"verb\" (specified
-- in the configuration) and are only activated when that verb begins
-- a message. All of the words in the message after the verb are
-- passed as an argument list.
newtype Command = Command  (forall channel user. Event channel user -> [Text] -> IO ())

-------------------------------------------------------------------------------
-- Errors

-- | An error in the core.
data CoreError
  = BackendNameClash !BackendName
  -- ^ A backend was added where the name was already taken.
  | BackendUnknown !BackendName
  -- ^ A backend was requested but it is unknown.
  | BackendBadConfig !BackendName !Text !Text
  -- ^ The configuration for a backend is invalid.
  | PluginNameClash !PluginName
  -- ^ A plugin was added where the name was already taken.
  | PluginUnknown !BackendName !Text !PluginName
  -- ^ A plugin was requested but it is unknown.
  | PluginBadConfig !BackendName !Text !PluginName !Text
  -- ^ The configuration for a plugin is invalid.
  | MonitorUnknown !BackendName !Text !PluginName !MonitorName
  -- ^ A monitor was requested but it is unknown.
  | CommandUnknown !BackendName !Text !PluginName !CommandName
  -- ^ A command was requested but it is unknown.
  deriving (Eq, Ord, Read, Show)
