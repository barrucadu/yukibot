-- |Functions for dealing with a client connected to multiple IRC
-- networks.
module Network.IRC.Asakura
    ( createAndRun
    , createAndRunWithTLS
    , createAndRunWithTLS'
    , addGlobalEventHandler
    ) where

import Control.Applicative        ((<$>))
import Control.Concurrent         (forkIO)
import Control.Concurrent.STM     (STM, TVar, atomically, readTVar, retry, writeTVar)
import Control.Monad              (unless)
import Control.Monad.IO.Class     (MonadIO, liftIO)
import Control.Monad.Trans.Reader (ask, runReaderT)
import Data.Text                  (Text)
import Network                    (HostName)
import Network.TLS                (Cipher)
import Network.IRC.Asakura.Types  (Bot, BotState(..), newBotState)
import Network.IRC.IDTE           (connect, connectWithTLS, connectWithTLS', defaultIRCConf, start')
import Network.IRC.IDTE.Types     (EventHandler, ConnectionConfig(..), InstanceConfig(..), IRCState, getInstanceConfig, newIRCState)

-- |Create and run a new bot, connecting to the given initial
-- network. If connecting fails, this returns the error
-- message. Otherwise, this blocks until all networks are disconnected
-- from.
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

-- |Start the bot with an initial configuration
start :: MonadIO m => ConnectionConfig -> InstanceConfig -> m ()
start cconf iconf = newBotState >>= liftIO . runReaderT (runner cconf iconf)

-- |The main event loop of the bot, starting out with connecting to a
-- network.
runner :: ConnectionConfig -> InstanceConfig -> Bot ()
runner cconf iconf = do
  let network = _server cconf

  state <- ask

  -- Fork off the client into its own thread
  let connections = _connections state
  let cconf' = cconf { _disconnect = liftIO $ asakuraDisconnectHandler network connections }
  conf <- newIRCState cconf' iconf
  liftIO . forkIO . start' $ conf

  -- And add to the list
  liftIO . atomically $ writeTVar (_connections state) [(network, conf)]

  -- Block until there are no more networks.
  liftIO . atomically $ do
    connections <- readTVar . _connections $ state
    unless (null connections) retry

-- |Remove the named network from the list
asakuraDisconnectHandler :: HostName -> TVar [(HostName, a)] -> IO ()
asakuraDisconnectHandler host tvar = atomically $ do
  networks <- readTVar tvar
  writeTVar tvar $ filter ((/=host) . fst) networks

-- |Add a new event handler to all networks, and to the list of event
-- handlers added to new connections by default.
addGlobalEventHandler :: EventHandler -> Bot ()
addGlobalEventHandler h = do
  tvarC  <- _connections <$> ask
  tvarDH <- _defHandlers <$> ask

  -- Atomically get all current networks, add the handler to them, and
  -- then add it to the defaults.
  liftIO . atomically $ do
    connections <- readTVar tvarC
    defaults    <- readTVar tvarDH

    mapM_ (addLocalEventHandler h . snd) connections

    writeTVar tvarDH $ h : defaults

-- |Add an event handler to a single IRC client state
addLocalEventHandler :: EventHandler -> IRCState -> STM ()
addLocalEventHandler h state = do
  -- Get the instance config of the specific IRC client
  let tvarIC = getInstanceConfig state
  iconf <- readTVar tvarIC

  -- And update
  let iconf' = iconf { _eventHandlers = h : _eventHandlers iconf }
  writeTVar tvarIC iconf'
