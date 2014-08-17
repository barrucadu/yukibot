-- |IRC client types.
module Network.IRC.IDTE.Client where

import Control.Monad.Trans.Class  (lift)
import Control.Monad.Trans.Reader (ReaderT, ask)
import Control.Monad.Trans.State  (StateT, get, put)
import Data.Text       (Text, pack)
import Network         (HostName, PortID)
import Network.TLS     (Context)
import System.IO       (Handle)

-- *State

-- |The IRC monad: read-only connection configuration and mutable
-- instance configuration.
type IRC a = ReaderT ConnectionConfig (StateT InstanceConfig IO) a

-- |Access the connection config
connectionConfig :: IRC ConnectionConfig
connectionConfig = ask

-- |Access the instance config.
instanceConfig :: IRC InstanceConfig
instanceConfig = lift get

-- |Update the instance config.
putInstanceConfig :: InstanceConfig -> IRC ()
putInstanceConfig = lift . put

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
