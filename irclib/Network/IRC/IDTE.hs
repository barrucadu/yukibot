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
import Control.Monad          (forever)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Crypto.Random.AESCtr   (makeSystem)
import Data.ByteString        (ByteString)
import Data.ByteString.Char8  (pack)
import Data.ByteString.Lazy   (fromChunks)
import Data.Default           (def)
import Data.Text.Encoding     (encodeUtf8)
import Network                (HostName, PortID, connectTo)
import Network.IRC            (Message, encode, decode)
import Network.IRC.IDTE.Types (IRC(..), IRCConf(..))
import Network.TLS
import Network.TLS.Extra      (ciphersuite_all)
import System.IO

import qualified Network.IRC as I

-- *Connecting to an IRC network

-- |Connect to a server without TLS.
connect :: MonadIO m => IRCConf -> HostName -> PortID -> m IRC
connect conf host port = do
  h <- liftIO $ do
    h' <- connectTo host port
    hSetEncoding    h' utf8
    hSetBuffering   h' NoBuffering
    hSetNewlineMode h' nativeNewlineMode
    return h'

  return IRC { _handle   = h
             , _tls      = Nothing
             , _server   = host
             , _port     = port
             , _nick     = _cnick conf
             , _username = _cusername conf
             , _realname = _crealname conf
             , _channels = []
             , _ctcpVer  = _cctcpver conf
             }

-- |Connect to a server with TLS.
connectWithTLS :: MonadIO m => IRCConf -> HostName -> PortID -> m IRC
connectWithTLS conf host port = connectWithTLS' conf host port defaultCiphers

-- |Connect to a server without TLS, supplying your own list of
-- ciphers, ordered by preference.
connectWithTLS' :: MonadIO m => IRCConf -> HostName -> PortID -> [Cipher] -> m IRC
connectWithTLS' conf host port ciphers = do
  -- Get an unencrypted connection
  irc <- connect conf host port

  -- And add a TLS context to it
  -- TODO: Do we want a better way to choose the bytes?
  tls <- addTLS host (pack "deadbeef") (_handle irc) ciphers

  return $ irc { _tls = Just tls }

-- |Run the event loop for a server, receiving messages and handing
-- them off to handlers as appropriate.
-- 
-- TODO: Any of that.
run :: (Functor m, MonadIO m) => IRC -> m ()
run irc = do
  -- Set the nick
  -- TODO: Mangle the nick until we get a unique one
  send irc . I.nick . encodeUtf8 . _nick $ irc

  -- Event loop
  liftIO . forever $ do
    msg <- recv irc
    -- TODO: Give to event handler (todo: event handlers)
    liftIO $ print msg

-- *Disconnecting

-- |Disconnect from a server, properly tearing down the TLS session
-- (if there is one).
disconnect :: MonadIO m => IRC -> m ()
disconnect irc = do
  case _tls irc of
    Just ctx -> bye ctx
    Nothing  -> return ()

  liftIO . hClose . _handle $ irc

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
send :: MonadIO m => IRC -> Message -> m ()
send irc msg = let h    = _handle irc
                   msg' = encode msg
               in case _tls irc of
                    Just ctx -> sendData ctx $ fromChunks [msg']
                    Nothing  -> liftIO $ hPrint h msg' >> hPrint h "\r\n"

-- |Receive a plain message. This blocks.
recv :: (Functor m, MonadIO m) => IRC -> m (Maybe Message)
recv irc = decode <$>
             case _tls irc of
               Just ctx -> recvData ctx
               Nothing  -> pack <$> (liftIO . hGetLine . _handle $ irc)
