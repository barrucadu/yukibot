-- |Functions for dealing with event handlers.
module Network.IRC.Asakura.Events
 ( -- *Adding event handlers
   addGlobalEventHandler
 , addGlobalEventHandler'
 , addDefaultHandlers
  -- *Constructing event handlers
 , runEverywhere
 , runAlways
 ) where

import Control.Concurrent.STM     (STM, atomically, readTVar, writeTVar)
import Control.Monad              (join)
import Control.Monad.IO.Class     (MonadIO, liftIO)
import Control.Monad.Trans.Reader (ask, runReaderT)
import Data.Text                  (Text)
import Network                    (HostName)
import Network.IRC.Asakura.Types
import Network.IRC.IDTE.Types     (Event(..), EventHandler, ConnectionConfig(..), InstanceConfig(..), IRCState, Source(..))

import qualified Network.IRC.IDTE.Types as IT

-- *Adding event handlers

-- |Add a new event handler to all networks, and to the list of event
-- handlers added to new connections by default.
addGlobalEventHandler :: AsakuraEventHandler -> Bot ()
addGlobalEventHandler h = do
  state  <- ask
  addGlobalEventHandler' state h

-- |Like 'addGlobalEventHandler', but takes the bot state as a
-- parameter and runs in IO.
addGlobalEventHandler' :: MonadIO m => BotState -> AsakuraEventHandler -> m ()
addGlobalEventHandler' state h = liftIO . atomically $ addGlobalEventHandlerSTM state h

-- |Add the default handlers to an IRC client.
addDefaultHandlers :: IRCState -> Bot ()
addDefaultHandlers ircstate = do
  state <- ask
  liftIO . atomically $ do
    defaults <- readTVar . _defHandlers $ state
    mapM_ (\h -> addLocalEventHandler (demote state h) ircstate) defaults

-- |Add an event handler to all networks.
addGlobalEventHandlerSTM :: BotState -> AsakuraEventHandler -> STM ()
addGlobalEventHandlerSTM state h = do
  -- Atomically get all current networks, add the handler to them, and
  -- then add it to the defaults.
  connections <- readTVar . _connections $ state
  defaults    <- readTVar . _defHandlers $ state

  mapM_ (addLocalEventHandler (demote state h) . snd) connections

  writeTVar (_defHandlers state) $ h : defaults

-- |Add an event handler to a single IRC client state
addLocalEventHandler :: (IRCState -> EventHandler) -> IRCState -> STM ()
addLocalEventHandler h state = do
  -- Get the instance config of the specific IRC client
  let tvarIC = IT.getInstanceConfig state
  iconf <- readTVar tvarIC

  -- And update
  let iconf' = iconf { _eventHandlers = h state : _eventHandlers iconf }
  writeTVar tvarIC iconf'

-- |Demote an Asakura event handler to an IDTE event handler.
demote :: BotState -> AsakuraEventHandler -> IRCState -> EventHandler
demote state h ircstate = IT.EventHandler
                            { IT._description = _description h
                            , IT._matchType   = _matchType h
                            , IT._eventFunc   = \e -> join . liftIO $ runReaderT (demoted e) state
                            }
    where network = _server $ IT.getConnectionConfig ircstate
          demoted ev = do
            active <- case _source ev of
                       Channel _ c -> _appliesTo h network c
                       _           -> _appliesDef h network

            if active
            then _eventFunc h ircstate ev
            else return $ return ()

-- *Constructing event handlers

-- |Helper for when you want an event handler to run in all channels.
runEverywhere :: HostName -> Text -> Bot Bool
runEverywhere _ _ = return True

-- |Helper for when you want an event handler to run always outside of
-- channels.
runAlways :: HostName -> Bot Bool
runAlways _ = return True
