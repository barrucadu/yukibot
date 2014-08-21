-- |Functions for dealing with a client connected to multiple IRC
-- networks.
module Network.IRC.Asakura
    ( -- *Initialisation
      createAndRun
    , createAndRunWithTLS
    , createAndRunWithTLS'
    -- *Running bots
    , start
    , run
    -- *Networks
    , addNetwork
    -- *Events
    , addGlobalEventHandler
    , addGlobalEventHandler'
    , runEverywhere
    , runAlways
    ) where

import Control.Concurrent         (forkIO)
import Control.Concurrent.STM     (TVar, atomically, readTVar, retry, writeTVar)
import Control.Monad              (unless)
import Control.Monad.IO.Class     (MonadIO, liftIO)
import Control.Monad.Trans.Reader (ask, runReaderT)
import Data.Text                  (Text)
import Network                    (HostName)
import Network.TLS                (Cipher)
import Network.IRC.Asakura.Events (addDefaultHandlers, addGlobalEventHandler, addGlobalEventHandler', runEverywhere, runAlways)
import Network.IRC.Asakura.Types
import Network.IRC.IDTE           (connect, connectWithTLS, connectWithTLS', defaultIRCConf, start')
import Network.IRC.IDTE.Types     (ConnectionConfig(..), InstanceConfig(..), newIRCState)

-- *Initialisation

-- |Create a new bot, connecting to the given initial network. If
-- connecting fails, this returns the error message. Otherwise, this
-- blocks until all networks are disconnected from.
createAndRun :: MonadIO m => HostName -> Int -> Either Text InstanceConfig -> m (Maybe String)
createAndRun host port inst = connect host port >>= flip runWith inst

-- |Like 'createAndRun', but connect with the default TLS configuration
createAndRunWithTLS :: MonadIO m => HostName -> Int -> Either Text InstanceConfig -> m (Maybe String)
createAndRunWithTLS host port inst = connectWithTLS host port >>= flip runWith inst

-- |Like 'createAndRunWithTLS', but take a list of ciphers.
createAndRunWithTLS' :: MonadIO m => HostName -> Int -> [Cipher] -> Either Text InstanceConfig -> m (Maybe String)
createAndRunWithTLS' host port ciphers inst = connectWithTLS' host port ciphers >>= flip runWith inst

-- |Create and run a bot with the given connection configuration
runWith :: MonadIO m => Either String ConnectionConfig -> Either Text InstanceConfig -> m (Maybe String)
runWith (Left str) _                = return $ Just str
runWith (Right cconf) (Left nick)   = start cconf (defaultIRCConf nick) >> return Nothing
runWith (Right cconf) (Right iconf) = start cconf iconf >> return Nothing

-- *Running bots

-- |Start the bot with an initial configuration
start :: MonadIO m => ConnectionConfig -> InstanceConfig -> m ()
start cconf iconf = newBotState >>= run cconf iconf

-- |Run the supplied bot configuration
run :: MonadIO m => ConnectionConfig -> InstanceConfig -> BotState -> m ()
run cconf iconf state = liftIO $ runReaderT (runner cconf iconf) state

-- *Event loop

-- |The main event loop of the bot, starting out with connecting to a
-- network.
runner :: ConnectionConfig -> InstanceConfig -> Bot ()
runner cconf iconf = do
  state <- ask

  -- Add the initial network
  addNetwork cconf iconf

  -- Block until there are no more networks.
  liftIO . atomically $ do
    connections <- readTVar . _connections $ state
    unless (null connections) retry

-- *Networks

-- |Add an initialised network to the bot: give it the default event
-- handlers and set up the disconnect handler.
addNetwork :: ConnectionConfig -> InstanceConfig -> Bot ()
addNetwork cconf iconf = do
  let network = _server cconf

  state <- ask

  -- Construct the initial state
  let cconf' = cconf { _disconnect = liftIO . asakuraDisconnectHandler network . _connections $ state }
  ircstate <- newIRCState cconf' iconf
  addDefaultHandlers ircstate

  -- Fork off the client into its own thread
  liftIO . forkIO . start' $ ircstate

  -- And add to the list
  liftIO . atomically $ writeTVar (_connections state) [(network, ircstate)]

-- *Handlers

-- |Remove the named network from the list
asakuraDisconnectHandler :: HostName -> TVar [(HostName, a)] -> IO ()
asakuraDisconnectHandler host tvar = atomically $ do
  networks <- readTVar tvar
  writeTVar tvar $ filter ((/=host) . fst) networks
