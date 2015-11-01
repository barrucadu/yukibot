-- |Types for IRC bots.
module Network.IRC.Bot.Types where

import Control.Concurrent.STM (TVar, atomically, newTVar)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Aeson.Types (Value, emptyObject)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Network.IRC.Client.Types (EventType, StatefulIRC, IRCState, UnicodeEvent)

-- *State

-- |The bot: a thin wrapper over some globally shared state.
type Bot a = StatefulBot () a

-- |The bot monad, with state.
type StatefulBot s a = ReaderT (BotState s) IO a

data BotState s = BotState
  { _connections :: TVar [(ByteString, IRCState s)]
  -- ^We abstract over a particular IRC client by instead dealing with
  -- a map from network names to individual client states, which we
  -- can update in order to communicate with clients.
  --
  -- Furthermore, this is behind a TVar so we can connect to new
  -- things at runtime.
  , _defHandlers :: TVar [EventHandler s]
  -- ^Default event handlers added to all new connections.
  , _config :: Value
  -- ^Read-only parsed global configuration.
  }

-- |Construct a new bot state
newBotState :: MonadIO m => m (BotState s)
newBotState = newBotState' emptyObject

-- |Construct a new bot state with some global config.
newBotState' :: MonadIO m => Value -> m (BotState s)
newBotState' cfg = do
  tvarC  <- liftIO . atomically . newTVar $ []
  tvarDH <- liftIO . atomically . newTVar $ []
  return BotState { _connections = tvarC, _defHandlers = tvarDH, _config = cfg }

-- *Events

-- |An irc-client event handler, lifted to run in the Asakura
-- context. Fields are as in irc-client.
data EventHandler s = EventHandler
  { _description :: Text
  , _matchType   :: EventType
  , _eventFunc   :: IRCState s -> UnicodeEvent -> StatefulBot s (StatefulIRC s ())
  , _appliesTo   :: ByteString -> Text -> StatefulBot s Bool
  -- ^Check if the event handler applies to this network/channel
  , _appliesDef  :: ByteString -> StatefulBot s Bool
  -- ^Whether the handler applies outside of a channel
  }
