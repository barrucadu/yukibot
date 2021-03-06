{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      : Yukibot.Types
-- Copyright   : (c) 2016 Michael Walker
-- License     : MIT
-- Stability   : experimental
-- Portability : DeriveFunctor, GADTs, GeneralizedNewtypeDeriving
module Yukibot.Types where

import Control.Concurrent.STM (TQueue, TVar)
import Control.Monad.Catch (Exception)
import Control.Monad.Trans.Free (FreeT)
import Data.Bson (Val)
import Data.ByteString (ByteString)
import Data.Hashable (Hashable(..))
import Data.HashMap.Strict (HashMap)
import Data.String (IsString)
import Data.Text (Text)
import qualified Database.MongoDB as M

-------------------------------------------------------------------------------
-- * Events

newtype ChannelName = ChannelName { getChannelName :: Text }
  deriving (Eq, Ord, Read, Show, Hashable, IsString, Val)

newtype UserName = UserName { getUserName :: Text }
  deriving (Eq, Ord, Read, Show, Hashable, IsString, Val)

data Event = Event
  { eventHandle  :: BackendHandle
  , eventWhoAmI  :: UserName
  , eventChannel :: Maybe ChannelName
  , eventUser    :: UserName
  , eventMessage :: Text
  }

-------------------------------------------------------------------------------
-- * Actions

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
-- * Backends

-- | Functor used to construct the 'BackendM' monad.
-- 'BackendF'/'BackendM' actions operate in the context of a single
-- command or monitor responding to a single event: there are no
-- long-lived actions.
data BackendF a
  = SendAction Action a
  -- ^ Send an 'Action' to the backend.
  | Reply Text a
  -- ^ Reply (@Say@ or @Whisper@) to the originator of the event.
  | QuickReply Text a
  -- ^ Send a message to the channel in which the event originated,
  -- but without addressing the specific user.
  | GetCommandPrefixIn (Maybe ChannelName) (Text -> a)
  -- ^ Get the command prefix in a channel. @Nothing@ indicates
  -- whispers.
  | GetDisabledPluginsIn (Maybe ChannelName) ([PluginName] -> a)
  -- ^ Get the disabled plugins in a channel.
  | GetEnabledCommandsIn (Maybe ChannelName) ([(PluginName, CommandName, Text)] -> a)
  -- ^ Get the enabled commands and verbs in a channel.
  | GetEnabledMonitorsIn (Maybe ChannelName) ([(PluginName, MonitorName)] -> a)
  -- ^ Get the enabled monitors in a channel.
  | GetInstance (InstantiatedBackend -> a)
  -- ^ Get the instantiated backend.
  | GetEvent (Event -> a)
  -- ^ Get the current event.
  | GetDeities ([UserName] -> a)
  -- ^ Get the deities of this backend.
  | GetBackendSig (BackendSig -> a)
  -- ^ Get the \"backend signature\". This can be used to uniquely
  -- identify a backend in the configuration (as long as, e.g.,
  -- backend arrays aren't re-ordered).
  | QueryMongo M.Selector M.Order ([M.Document] -> a)
  -- ^ Query the MongoDB collection for this plugin. Each plugin has
  -- its own collection.
  --
  -- This automatically namespaces by backend signature.
  | InsertMongo [M.Document] a
  -- ^ Insert into the MongoDB collection.
  --
  -- This automatically namespaces by backend signature.
  | UpsertMongo M.Selector M.Document a
  -- ^ Upsert a value in the MongoDB collection: replace the first
  -- document in the selection if there is one; otherwise insert a new
  -- document.
  --
  -- This automatically namespaces by backend signature.
  | DeleteMongo M.Selector a
  -- ^ Delete from the MongoDB collection.
  --
  -- This automatically namespaces by backend signature.
  | DoMongo (M.Collection -> M.Action IO ()) a
  -- ^ Perform an arbitrary sequence of operations against the MongoDB
  -- collection.
  --
  -- This does NOT automatically namespace things by the backend
  -- signature! Make sure to appropriately manage the \"_backendsig\"
  -- field of any documents if you want this scoping!
  deriving Functor

-- | The monad in commands and monitors operate.
type BackendM = FreeT BackendF IO

newtype BackendName = BackendName { getBackendName :: Text }
  deriving (Eq, Ord, Read, Show, Hashable, IsString, Val)

-- | A representation of a backend.
data Backend where
  Backend :: { initialise :: Logger -> ((BackendHandle -> Event) -> IO ()) -> IO a
             , run :: TQueue Action -> a -> IO ()
             , describe :: Text
             , logFile :: FilePath
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

-- | The \"signature\" of a backend. This can be used to uniquely
-- identify a backend in the config, as long as (e.g.) backend arrays
-- are not re-ordered. The three components are the backend name, the
-- name of the specific instance, and its position in the array. If
-- the instance is not in an array, the position is 0 (as if it were
-- at the start of an array).
type BackendSig = (BackendName, Text, Int)

-- | A handle to a backend, which can be used to interact with it.
data BackendHandle = BackendHandle
  { msgQueue     :: TQueue Action
  , hasStarted   :: TVar Bool
  , hasStopped   :: TVar Bool
  , description  :: Text
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
-- * Logging

-- | A logger of messages sent to and from the server by the backend.
data Logger = Logger
  { toServer :: ByteString -> IO ()
  -- ^ Log a message sent to the server.
  , fromServer :: ByteString -> IO ()
  -- ^ Log a message received from the server.
  , flushLog :: IO ()
  -- ^ Flush any buffered log entries to disk.
  , closeLog :: IO ()
  -- ^ Close the log, any further operations will fail.
  }

-- | A tag displayed in the stdout log to indicate which backend a
-- message is from. Tags are not included in the backend-specific log
-- files.
newtype Tag = Tag { getTag :: Text }
  deriving (Eq, Ord, Read, Show, Hashable, IsString, Val)

-------------------------------------------------------------------------------
-- * Plugins

newtype PluginName = PluginName { getPluginName :: Text }
  deriving (Eq, Ord, Read, Show, Hashable, IsString, Val)

newtype MonitorName = MonitorName { getMonitorName :: Text }
  deriving (Eq, Ord, Read, Show, Hashable, IsString, Val)

newtype CommandName = CommandName { getCommandName :: Text }
  deriving (Eq, Ord, Read, Show, Hashable, IsString, Val)

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
-- * Errors

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
  | MonitorNoSuchPlugin !BackendName !Text !PluginName !MonitorName
  -- ^ A monitor was requested from a plugin which doesn't exist.
  | MonitorMissingName !BackendName !Text !PluginName
  -- ^ A monitor was requested but no name given.
  | MonitorUnknown !BackendName !Text !PluginName !MonitorName
  -- ^ A monitor was requested but it is unknown.
  | CommandNoSuchPlugin !BackendName !Text !PluginName !CommandName !Text
  -- ^ A command was requested from a plugin which doesn't exist.
  | CommandMissingName !BackendName !Text !PluginName !Text
  -- ^ A command definition is missing the name of the command.
  | CommandBadFormat !BackendName !Text !Text
  -- ^ A command definition is something other than "<string> = <string>".
  | CommandMissingVerb !BackendName !Text !PluginName !CommandName
  -- ^ A command definition but has an empty verb.
  | CommandUnknown !BackendName !Text !PluginName !CommandName !Text
  -- ^ A command was requested but it is unknown.
  | DisabledMissingChannel !BackendName !Text
  -- ^ A disabled monitor declaration is missing the name of the channel.
  | DisabledBadFormat !BackendName !Text !ChannelName
  -- ^ A disabled monitor declaration is something other than "<string> = [<string>]"
  | DisabledError !BackendName !Text !ChannelName !CoreError
  -- ^ A disabled monitor declaration has some other error in it.
  deriving (Eq, Ord, Read, Show)
