-- |Entry point to the Integrated Data Thought Entity.
module Network.IRC.IDTE
    ( module Network.IRC.IDTE.Types
    , connect
    , connectWithTLS
    , connectWithTLS'
    , run
    , disconnect
    , defaultCiphers
    ) where

import Control.Applicative    ((<$>))
import Control.Monad          (forever, void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.RWS (ask, get, runRWST, tell)
import Crypto.Random.AESCtr   (makeSystem)
import Data.ByteString        (ByteString)
import Data.ByteString.Char8  (pack, unpack)
import Data.ByteString.Lazy   (fromChunks)
import Data.Default           (def)
import Data.Text.Encoding     (encodeUtf8)
import Data.Time.Clock        (getCurrentTime)
import Data.Time.Format       (formatTime)
import Network                (HostName, PortID, connectTo)
import Network.IRC            (Message, encode, decode)
import Network.IRC.IDTE.Types
import Network.TLS
import Network.TLS.Extra      (ciphersuite_all)
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

-- |Run the event loop for a server, receiving messages and handing
-- them off to handlers as appropriate.
-- 
-- TODO: Any of that.
run :: MonadIO m => ConnectionConfig -> InstanceConfig -> m ()
run cconf iconf = liftIO . void $ runRWST runner cconf iconf

-- |The event loop.
runner :: IRC ()
runner = do
  -- Set the nick
  -- TODO: Mangle the nick until we get a unique one
  send . I.nick . encodeUtf8 . _nick <$> get

  -- Event loop
  forever $ do
    msg <- recv
    case msg of
      -- TODO: Give to event handler (todo: event handlers)
      Just msg' -> logmsg msg'

      -- Ignore malformed messages
      Nothing   -> return ()

-- *Disconnecting

-- |Disconnect from a server, properly tearing down the TLS session
-- (if there is one).
disconnect :: IRC ()
disconnect = do
  tls <- _tls    <$> ask
  h   <- _handle <$> ask

  case tls of
    Just ctx -> bye ctx
    Nothing  -> return ()

  liftIO $ hClose h

-- |Log a message to stdout and the internal log
logmsg :: Message -> IRC ()
logmsg msg = do
  now <- liftIO $ getCurrentTime

  tell [(now, encode msg)]

  liftIO . putStrLn $ formatTime defaultTimeLocale "%c" now ++ unpack (encode msg)

-- *TLS

-- |Default allowable ciphers, ordered from strong to weak.
defaultCiphers :: [Cipher]
defaultCiphers = ciphersuite_all

-- |Enable a TLS context on the given socket
addTLS :: MonadIO m => HostName -> ByteString -> Handle -> [Cipher] -> m Context
addTLS host bytes h ciphers = do
  let supported = def { supportedCiphers = ciphers }
  let clientctx = (defaultParamsClient host bytes) { clientSupported = supported }

  rng <- liftIO makeSystem
  ctx <- contextNew h clientctx rng
  handshake ctx
  return ctx

-- *Messaging

-- |Send a plain message
-- TODO: Flood control
send :: Message -> IRC ()
send msg = do
  tls <- _tls    <$> ask
  h   <- _handle <$> ask

  let msg' = encode msg

  case tls of
    Just ctx -> sendData ctx $ fromChunks [msg']
    Nothing  -> liftIO $ hPrint h msg' >> hPrint h "\r\n"

-- |Receive a plain message. This blocks.
recv :: IRC (Maybe Message)
recv = do
  tls <- _tls    <$> ask
  h   <- _handle <$> ask

  decode <$>
    case tls of
      Just ctx -> recvData ctx
      Nothing  -> pack <$> liftIO (hGetLine h)
