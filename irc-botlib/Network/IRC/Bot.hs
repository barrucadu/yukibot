-- |Functions for dealing with a client connected to multiple IRC
-- networks.
module Network.IRC.Bot
  ( -- *Blocking
    block
  , blockWithState
  -- *Networks
  , addNetwork
  , addNetworkStateful
  -- *Events
  , addGlobalEventHandler
  , addGlobalEventHandler'
  , runEverywhere
  , runAlways
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM (TVar, atomically, readTVar, retry, writeTVar)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Reader (ask, runReaderT)
import Data.ByteString (ByteString)
import Network.IRC.Client (start')
import Network.IRC.Client.Types (ConnectionConfig(..), InstanceConfig(..), IRCState, newIRCState)

import Network.IRC.Bot.Events
import Network.IRC.Bot.Types

-- *Blocking

-- |Start the bot with an initial configuration
block :: MonadIO m => m ()
block = newBotState >>= blockWithState

-- |Run the supplied bot configuration
blockWithState :: MonadIO m => BotState s -> m ()
blockWithState = liftIO . runReaderT block'

-- |Block until all networks have been disconnected from
block' :: StatefulBot s ()
block' = do
  state <- ask

  -- Block until there are no more networks.
  liftIO . atomically $ do
    connections <- readTVar . _connections $ state
    unless (null connections) retry

-- *Networks

-- |Add an initialised network to the bot: give it the default event
-- handlers and set up the disconnect handler.
addNetwork :: ConnectionConfig () -> InstanceConfig () -> Bot (IRCState ())
addNetwork cconf iconf = addNetworkStateful cconf iconf ()

-- | Like  'addNetwork' but for clients with state.
addNetworkStateful :: ConnectionConfig s -> InstanceConfig s -> s -> StatefulBot s (IRCState s)
addNetworkStateful cconf iconf s = do
  let network = _server cconf

  state <- ask

  -- Construct the initial state
  let cconf' = cconf { _ondisconnect = liftIO . asakuraDisconnectHandler network . _connections $ state }
  ircstate <- newIRCState cconf' iconf s
  addDefaultHandlers ircstate

  -- Fork off the client into its own thread
  liftIO . forkIO . start' $ ircstate

  -- And add to the list
  liftIO . atomically $ writeTVar (_connections state) [(network, ircstate)]

-- Finally, return the new state
  return ircstate

-- *Handlers

-- |Remove the named network from the list
asakuraDisconnectHandler :: ByteString -> TVar [(ByteString, a)] -> IO ()
asakuraDisconnectHandler host tvar = atomically $ do
  networks <- readTVar tvar
  writeTVar tvar $ filter ((/=host) . fst) networks
