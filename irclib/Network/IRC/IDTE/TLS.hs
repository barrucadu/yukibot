-- |Functions for dealing with possibly encrypted sessions.
module Network.IRC.IDTE.TLS where

import Control.Applicative ((<$>))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Crypto.Random.AESCtr   (makeSystem)
import Data.ByteString        (ByteString)
import Data.ByteString.Lazy   (fromChunks)
import Data.Default           (def)
import Network                (HostName)
import Network.IRC            (Message, encode, decode)
import Network.IRC.IDTE.Types
import Network.TLS
import Network.TLS.Extra      (ciphersuite_all)
import System.IO              (Handle)

-- *Initialisation

-- |Default allowable ciphers, ordered from strong to weak.
defaultCiphers :: [Cipher]
defaultCiphers = ciphersuite_all

-- |Enable a TLS context on the given handle.
addTLS :: MonadIO m => HostName -> ByteString -> Handle -> [Cipher] -> m Context
addTLS host bytes h ciphers = do
  let supported = def { supportedCiphers = ciphers }
  let clientctx = (defaultParamsClient host bytes) { clientSupported = supported }

  rng <- liftIO makeSystem
  ctx <- contextNew h clientctx rng
  handshake ctx
  return ctx

-- *Using

-- |Run one of two functions depending on whether the connection is
-- encrypted or not.
withTLS :: (Context -> IRC a) -> (Handle -> IRC a) -> IRC a
withTLS tlsf plainf = do
  tls <- _tls    <$> connectionConfig
  h   <- _handle <$> connectionConfig

  case tls of
    Just ctx -> tlsf ctx
    Nothing  -> plainf h

-- |Run the provided function when there is a TLS context.
whenTLS :: (Context -> IRC ()) -> IRC ()
whenTLS tlsf = withTLS tlsf (const $ return ())

-- *Messaging

-- |Send a message.
sendTLS :: Message -> Context -> IRC ()
sendTLS msg ctx = sendData ctx $ fromChunks [encode msg]

-- |Receive a message.
recvTLS :: Context -> IRC (Maybe Message)
recvTLS = fmap decode . recvData

-- *Termination

-- |Close the TLS context, if there is one.
endTLS :: IRC ()
endTLS = whenTLS bye
