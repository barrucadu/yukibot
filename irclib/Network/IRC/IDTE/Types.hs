-- |IDTE-specific types. This is an internal module.
module Network.IRC.IDTE.Types where

import Data.Text   (Text, pack)
import Network     (HostName, PortID)
import Network.TLS (Context)
import System.IO   (Handle)

-- *Client state

-- |The state of an IRC client session.
data IRC = IRC { _handle   :: Handle
               -- ^Server connection handle
               , _tls      :: Maybe Context
               -- ^TLS context, if TLS was used.
               , _server   :: HostName
               -- ^The server host
               , _port     :: PortID
               -- ^The server port
               , _nick     :: Text
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

-- |The initial configuration of an IRC client
data IRCConf = IRCConf { _cnick     :: Text
                       -- ^The initial nick
                       , _cusername :: Text
                       -- ^The initial username
                       , _crealname :: Text
                       -- ^The initial realname
                       , _cctcpver  :: Text
                       -- ^The initial CTCP VERSION response
                       }

-- |Construct a default IRC configuration from a nick
defaultIRCConf :: Text -> IRCConf
defaultIRCConf nick = IRCConf { _cnick     = nick
                              , _cusername = nick
                              , _crealname = nick
                              , _cctcpver  = pack "idte-0.0.0.1"
                              }
