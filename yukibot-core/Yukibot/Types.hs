{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      : Yukibot.Types
-- Copyright   : (c) 2016 Michael Walker
-- License     : MIT
-- Stability   : experimental
-- Portability : DeriveFunctor, GADTs, GeneralizedNewtypeDeriving
module Yukibot.Types
  ( -- * Events
    ChannelName(..)
  , UserName(..)
  , Event(..)
  -- * Actions
  , Action(..)
  -- * Backends
  , BackendF(..)
  , BackendM
  , BackendName(..)
  , Backend(..)
  , InstantiatedBackend(..)
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
import Control.Monad.Trans.Free (FreeT)
import Data.ByteString (ByteString)
import Data.Hashable (Hashable(..))
import Data.HashMap.Strict (HashMap)
import Data.String (IsString)
import Data.Text (Text)

-------------------------------------------------------------------------------
-- Events

newtype ChannelName = ChannelName { getChannelName :: Text }
  deriving (Eq, Ord, Read, Show, Hashable, IsString)

newtype UserName = UserName { getUserName :: Text }
  deriving (Eq, Ord, Read, Show, Hashable, IsString)

data Event = Event
  { eventHandle  :: BackendHandle
  , eventWhoAmI  :: UserName
  , eventChannel :: Maybe ChannelName
  , eventUser    :: UserName
  , eventMessage :: Text
  }

-------------------------------------------------------------------------------
-- Actions

data Action
  = Join ChannelName
  -- ^ Join a new channel.
  | Leave ChannelName
  -- ^ Leave a current channel.
  | Say ChannelName [UserName] Text
  -- ^ Send a message to a channel, optionally addressed to a collection of users.
  | Whisper UserName Text
  -- ^ Send a message to a user.
  | Terminate
  -- ^ Gracefully disconnect.
  deriving (Eq, Read, Show)

-------------------------------------------------------------------------------
-- Backends

data BackendF a
  = SendAction Action a
  | Reply Text a
  | QuickReply Text a
  | GetCommandPrefixIn (Maybe ChannelName) (Text -> a)
  | GetDisabledPluginsIn (Maybe ChannelName) ([PluginName] -> a)
  | GetEnabledCommandsIn (Maybe ChannelName) ([(PluginName, CommandName, Text)] -> a)
  | GetEnabledMonitorsIn (Maybe ChannelName) ([(PluginName, MonitorName)] -> a)
  | GetInstance (InstantiatedBackend -> a)
  | GetEvent (Event -> a)
  | GetDeities ([UserName] -> a)
  deriving Functor

type BackendM = FreeT BackendF IO

newtype BackendName = BackendName { getBackendName :: Text }
  deriving (Eq, Ord, Read, Show, Hashable, IsString)

-- | A representation of a backend.
data Backend where
  Backend :: { initialise :: RawLogger -> ((BackendHandle -> Event) -> IO ()) -> IO a
             , run :: TQueue Action -> a -> IO ()
             , describe :: Text
             , rawLogFile :: FilePath
             , unrawLogFile :: FilePath
             } -> Backend

-- | An instantiated backend, corresponding to one entry in the
-- backend configuration.
data InstantiatedBackend = InstantiatedBackend
  { instBackendName  :: BackendName
  , instSpecificName :: Text
  , instIndex        :: Int
  , instBackend      :: Backend
  , instPlugins      :: [(PluginName, Plugin)]
  }

instance Hashable InstantiatedBackend where
  hashWithSalt salt ib = hashWithSalt salt (instBackendName ib, instSpecificName ib, instIndex ib)

-- | A handle to a backend, which can be used to interact with it.
data BackendHandle = BackendHandle
  { msgQueue     :: TQueue Action
  , hasStarted   :: TVar Bool
  , hasStopped   :: TVar Bool
  , description  :: Text
  , actionLogger :: Action -> IO ()
  , backendName  :: BackendName
  , specificName :: Text
  , backendIndex :: Int
  }

instance Eq BackendHandle where
  h1 == h2 = msgQueue h1 == msgQueue h2

data BackendTerminatedException = BackendTerminatedException
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

instance Exception BackendTerminatedException

-------------------------------------------------------------------------------
-- Logging

-- | A logger of events and actions received from and sent to the
-- backend by the bot.
data Logger = Logger
  { loggerEvent :: Event -> IO ()
    -- ^ Log an event received from the backend.
  , loggerAction :: Action -> IO ()
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
  { pluginHelp :: Text
  , pluginMonitors :: HashMap MonitorName Monitor
  , pluginCommands :: HashMap CommandName Command
  }

-- | Monitors are activated on every message. They can communicate
-- with the backend using the supplied 'Event'.
data Monitor = Monitor
  { monitorHelp :: Text
  , monitorAction :: Event -> BackendM ()
  }

-- | Commands are like monitors, but they have a \"verb\" (specified
-- in the configuration) and are only activated when that verb begins
-- a message. All of the words in the message after the verb are
-- passed as an argument list.
data Command = Command
  { commandHelp :: Text
  , commandAction :: Event -> [Text] -> BackendM ()
  }

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
