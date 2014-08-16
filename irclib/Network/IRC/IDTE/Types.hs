-- |IDTE-specific types.
module Network.IRC.IDTE.Types where

import Control.Monad.Trans.RWS (RWST)
import Data.ByteString (ByteString)
import Data.Text       (Text, pack)
import Data.Time.Clock (UTCTime)
import Network         (HostName, PortID)
import Network.TLS     (Context)
import System.IO       (Handle)

-- *Client state

-- |The IRC monad: read-only connection configuration, mutable
-- instance configuration, and a log of messages received.
type IRC a = RWST ConnectionConfig [(UTCTime, ByteString)] InstanceConfig IO a

-- |The state of an IRC server connection
data ConnectionConfig = ConnectionConfig
    { _handle :: Handle
    -- ^Server connection handle
    , _tls    :: Maybe Context
    -- ^TLS context, if TLS was used.
    , _server :: HostName
    -- ^The server host
    , _port   :: PortID
    -- ^The server port
    }

-- |The updateable state of an IRC connection
data InstanceConfig = InstanceConfig
    { _nick     :: Text
    -- ^Client nick
    , _username :: Text
    -- ^Client username
    , _realname :: Text
    -- ^Client realname
    , _channels :: [Text]
    -- ^Current channels
    , _ctcpVer  :: Text
    -- ^Response to CTCP VERSION
    }

-- *Initial configuration

-- |Construct a default IRC configuration from a nick
defaultIRCConf :: Text -> InstanceConfig
defaultIRCConf nick = InstanceConfig
                      { _nick     = nick
                      , _username = nick
                      , _realname = nick
                      , _channels = []
                      , _ctcpVer  = pack "idte-0.0.0.1"
                      }
