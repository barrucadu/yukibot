-- |Types for IRC bots.
module Network.IRC.Asakura.Types where

import Control.Concurrent.STM     (TVar, atomically, newTVar)
import Control.Monad.IO.Class     (MonadIO, liftIO)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Text                  (Text)
import Network                    (HostName)
import Network.IRC.IDTE.Types     (Event, EventType, IRC, IRCState)

-- *State

-- |The bot: a thin wrapper over some globally shared state.
type Bot a = ReaderT BotState IO a

data BotState = BotState
    { _connections :: TVar [(HostName, IRCState)]
    -- ^We abstract over a particular IRC client by instead dealing
    -- with a map from network names to individual client states,
    -- which we can update in order to communicate with clients.
    --
    -- Furthermore, this is behind a TVar so we can connect to new
    -- things at runtime.
    , _defHandlers :: TVar [AsakuraEventHandler]
    -- ^Default event handlers added to all new connections.
    }

-- |Construct a new bot state
newBotState :: MonadIO m => m BotState
newBotState = do
  tvarC  <- liftIO . atomically . newTVar $ []
  tvarDH <- liftIO . atomically . newTVar $ []
  return BotState { _connections = tvarC
                  , _defHandlers = tvarDH
                  }

-- *Events

-- |An IDTE event handler, lifted to run in the Asakura
-- context. Fields are as in IDTE.
data AsakuraEventHandler = AsakuraEventHandler
    { _description :: Text
    , _matchType   :: EventType
    , _eventFunc   :: IRCState -> Event -> Bot (IRC ())
    , _appliesTo   :: HostName -> Text -> Bot Bool
    -- ^Check if the event handler applies to this network/channel
    , _appliesDef  :: HostName -> Bot Bool
    -- ^Whether the handler applies outside of a channel
    }
