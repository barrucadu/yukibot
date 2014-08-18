-- |Entry point to the Integrated Data Thought Entity.
module Network.IRC.IDTE
    ( connect
    , connectWithTLS
    , connectWithTLS'
    , run
    , disconnect
    ) where

import Control.Applicative    ((<$>))
import Control.Monad          (forever, void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Reader (runReaderT)
import Control.Monad.Trans.State  (runStateT)
import Data.ByteString.Char8  (pack, unpack)
import Data.Text.Encoding     (encodeUtf8)
import Data.Time.Clock        (getCurrentTime)
import Data.Time.Format       (formatTime)
import Network                (HostName, PortID, connectTo)
import Network.IRC            (Message, encode, decode)
import Network.IRC.IDTE.Events (toEvent)
import Network.IRC.IDTE.TLS
import Network.IRC.IDTE.Types
import Network.TLS            (Cipher)
import System.Locale          (defaultTimeLocale)
import System.IO

import qualified Network.IRC as I

-- *Connecting to an IRC network

-- |Connect to a server without TLS.
connect :: MonadIO m => HostName -> PortID -> m ConnectionConfig
connect host port = do
  h <- liftIO $ do
    h' <- connectTo host port
    hSetEncoding    h' utf8
    hSetBuffering   h' NoBuffering
    hSetNewlineMode h' nativeNewlineMode
    return h'

  return ConnectionConfig
             { _handle   = h
             , _tls      = Nothing
             , _server   = host
             , _port     = port
             }

-- |Connect to a server with TLS.
connectWithTLS :: MonadIO m => HostName -> PortID -> m ConnectionConfig
connectWithTLS host port = connectWithTLS' host port defaultCiphers

-- |Connect to a server without TLS, supplying your own list of
-- ciphers, ordered by preference.
connectWithTLS' :: MonadIO m => HostName -> PortID -> [Cipher] -> m ConnectionConfig
connectWithTLS' host port ciphers = do
  -- Get an unencrypted connection
  irc <- connect host port

  -- And add a TLS context to it
  -- TODO: Do we want a better way to choose the bytes?
  tls <- addTLS host (pack "deadbeef") (_handle irc) ciphers

  return $ irc { _tls = Just tls }

-- *Event loop

-- |Run the event loop for a server, receiving messages and handing
-- them off to handlers as appropriate.
run :: MonadIO m => ConnectionConfig -> InstanceConfig -> m ()
run cconf iconf = liftIO . void . flip runStateT iconf $ runReaderT runner cconf

-- |The event loop.
runner :: IRC ()
runner = do
  -- Set the nick
  -- TODO: Mangle the nick until we get a unique one
  send . I.nick . encodeUtf8 . _nick <$> instanceConfig

  -- Event loop
  forever $ do
    msg <- recv
    case msg of
      Just msg' -> do
        logmsg msg'

        event <- toEvent msg' send

        handlers <- getHandlersFor event . _eventHandlers <$> instanceConfig
        -- TODO: Parallelise this (requires bunging state behind an MVar)
        mapM_ ($ event) handlers

      -- Ignore malformed messages
      Nothing   -> return ()

-- |Get the event handlers for an event.
getHandlersFor :: Event -> [(EventType, Event -> IRC ())] -> [Event -> IRC ()]
getHandlersFor e = map snd . if ety == EEverything
                             then id
                             else filter $ (== ety) . fst
    where ety = _eventType e

-- |Log a message to stdout and the internal log
logmsg :: Message -> IRC ()
logmsg msg = do
  now <- liftIO getCurrentTime

  liftIO . putStrLn $ formatTime defaultTimeLocale "%c" now ++ unpack (encode msg)

-- *Messaging

-- |Send a message, using TLS if enabled.
-- TODO: Flood control
send :: Message -> IRC ()
send msg = withTLS (sendTLS msg)
                   (\h -> liftIO $ hPrint h (encode msg) >> hPrint h "\r\n")

-- |Receive a message, using TLS if enabled. This blocks.
recv :: IRC (Maybe Message)
recv = withTLS recvTLS (fmap (decode . pack) . liftIO . hGetLine)

-- *Disconnecting

-- |Disconnect from a server, properly tearing down the TLS session
-- (if there is one).
disconnect :: IRC ()
disconnect = do
  h <- _handle <$> connectionConfig
  endTLS
  liftIO $ hClose h
