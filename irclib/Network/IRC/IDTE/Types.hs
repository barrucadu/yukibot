-- |Types for IRC clients, because GHC doesn't do recursive modules
-- well.
module Network.IRC.IDTE.Types where

import Control.Monad.Trans.Class  (lift)
import Control.Monad.Trans.Reader (ReaderT, ask)
import Control.Monad.Trans.State  (StateT, get, put)
import Data.Text                  (Text)
import Network                    (HostName, PortID)
import Network.IRC                (Message)
import Network.TLS                (Context)
import System.IO                  (Handle)

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
    , _eventHandlers :: [EventHandler]
    -- ^The registered event handlers
    }

-- *Events

-- |An event has a message, some information on the source, and a
-- reply function.
data Event = Event
    { _rawMessage :: Message
    -- ^The original message, split into parts and nothing more.
    , _eventType  :: EventType
    -- ^The type of the event, as registered by event handlers.
    , _source     :: Source
    -- ^The source of the message.
    , _message    :: IrcMessage
    -- ^The message data, split into a sum type.
    , _reply      :: IrcMessage -> IRC ()
    -- ^Sends a message to the source of this event.
    , _send       :: Source -> IrcMessage -> IRC ()
    -- ^Send a message
    }

-- |Types of events which can be caught.
data EventType = EEverything
               -- ^Match all events
               | ENothing
               -- ^Match no events
               | EPrivmsg | ENotice | ECTCP | ENick | EJoin | EPart | EQuit | EMode | ETopic | EInvite | EKick | EPing | ENumeric
               deriving Eq

-- |A function which handles an event.
data EventHandler = EventHandler
    { _description :: Text
    -- ^A description of the event handler
    , _matchType   :: EventType
    -- ^Which type to be triggered by
    , _eventFunc   :: Event -> IRC ()
    -- ^The function to call
    }

-- |The source of a message.
data Source = Server
            -- ^The message comes from the server.
            | Channel Text Text
            -- ^A channel the client is in. The first Text is the
            -- nick, the second, the channel name.
            | User Text
            -- ^A query from a user.
            | UnknownSource
            -- ^The source could not be determined, see the raw
            -- message

-- |A decoded message
data IrcMessage = Privmsg Text
                -- ^The client has received a message, which may be to
                -- a channel it's in.
                --
                -- CTCPs will, however, not raise this event (despite
                -- being sent as a PRIVMSG).

                | Notice Text
                -- ^Like a PRIVMSG, except an automatic reply must
                -- *not* be generated.

                | CTCP Text [Text]
                -- ^A CTCP has been received.

                | Nick Text
                -- ^Someone has updated their nick. The given nickname
                -- is the new one.

                | Join Text
                -- ^Someone has joined a channel the client is in.

                | Part Text (Maybe Text)
                -- ^Someone has left a channel the client is in.

                | Quit Text (Maybe Text)
                -- ^Someone has quit a channel the client is in.

                | Mode [ModeChange]
                -- ^Some mode changes have been applied to a channel
                -- the client is in, or a user in a channel the client
                -- is in.

                | Topic Text
                -- ^The topic of a channel the client is in has been
                -- updated.

                | Invite Text Text
                -- ^The client has been invited to a channel. The
                -- first text is the nick of the inviter.

                | Kick Text (Maybe Text)
                -- ^Someone has been kicked from a channel the client
                -- is in.

                | Ping Text
                -- ^A ping has been received (probably from the
                -- server, due to a period of inactivity). The Text is
                -- where the PONG should be sent.

                | Numeric Int [Text]
                -- ^One of the many numeric codes has been received in
                -- response to something the client did.

                | UnknownMessage
                -- ^The message could not be decoded, see the raw
                -- message.

-- |A single mode change to a channel or user.
data ModeChange = ModeChange
    { _set      :: Bool
    -- ^Whether the mode has been enabled or disabled
    , _flag     :: Text
    -- ^The mode name
    , _modeargs :: Maybe [Text]
    -- ^Any arguments to the mode change
    }
