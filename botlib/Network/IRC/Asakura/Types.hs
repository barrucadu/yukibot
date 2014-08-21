-- |Types for IRC bots.
module Network.IRC.Asakura.Types where

import Control.Concurrent.STM     (TVar, atomically, newTVar, readTVar, writeTVar)
import Control.Monad.IO.Class     (MonadIO, liftIO)
import Control.Monad.Trans.Reader (ReaderT)
import Network                    (HostName)
import Network.IRC.IDTE.Types     (EventHandler, IRCState)

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
    , _defHandlers :: TVar [EventHandler]
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
