{-# LANGUAGE OverloadedStrings #-}

-- |Provides commands to deal with multiple networks
module Yukibot.Plugins.Networks
    ( connectCmd
    , disconnectCmd
    --, shout
    -- *Integration
    , connectTo
    , splitNetwork
    ) where

import Control.Applicative      ((<$>))
import Control.Monad            (when, void)
import Data.ByteString          (ByteString)
import Data.ByteString.Char8    (pack)
import Data.Monoid              ((<>))
import Data.Text                (Text, breakOn, unpack)
import Network.IRC.Asakura      (addNetwork)
import Network.IRC.Asakura.Commands (CommandDef(..))
import Network.IRC.Asakura.Types (Bot)
import Network.IRC.Client       (Message(Numeric, Privmsg, Join, Quit), connectWithTLS, defaultIRCConf, send, reply)
import Network.IRC.Client.Types (EventType(ENumeric), Event(_message), EventHandler(..), InstanceConfig(_eventHandlers))
import Text.Read                (readMaybe)

import qualified Data.Text as T

-- |Connect to another network
-- TODO: Try TLS and, on failure, fallback to non-TLS
connectCmd :: CommandDef
connectCmd = CommandDef
             { _verb = ["connect"]
             , _help = "<network>[:<port>] [<nick> [<nickserv password>]] - connect to the given network, use the nick 'yukibot' if not specified."
             , _action = go
             }
  where
    go [network]                 _ _ = go' network "yukibot" Nothing
    go [network, nick]           _ _ = go' network nick      Nothing
    go [network, nick, nickserv] _ _ = go' network nick    $ Just nickserv
    go _ _ ev = return $ reply ev "Which network?"

    go' network nick nickserv =
      let (net, port) = splitNetwork network
      in do
        connectTo net port nick nickserv []
        return $ return ()

-- |Disconnect from the current network
disconnectCmd :: CommandDef
disconnectCmd = CommandDef
                { _verb = ["disconnect"]
                , _help = "[<message>] - Disconnect from the current network. Disconnecting from all networks terminates yukibot."
                , _action = go
                }
  where
    go []  _ _ = return . send $ Quit Nothing
    go msg _ _ = return . send . Quit . Just $ T.unwords msg

-- *Integration

-- |Connect to a network, possibly authing with nickserv and joining some channels.
--
-- TODO: Try TLS, then fallback to plaintext if that fails. Requires
-- some sort of connectTryTLS in irc-conduit, which catches a TLS
-- exception and retries with plaintext on failure.
connectTo :: ByteString -> Int -> Text -> Maybe Text -> [Text] -> Bot ()
connectTo network port nick nickserv chans = do
  -- Set up the initial state
  cconf <- connectWithTLS network port 1
  let iconf  = defaultIRCConf nick
  let iconf' = iconf { _eventHandlers = onWelcome : _eventHandlers iconf }

  -- Add it to the bot
  void $ addNetwork cconf iconf'

  where
    onWelcome = EventHandler { _description = "Apply initial state"
                             , _matchType   = ENumeric
                             , _eventFunc   = go
                             }

    go ev =
      let Numeric n _ = _message ev
      in when (n == 1) $ do
        -- Auth with nickserv
        case nickserv of
          Just pass -> send . Privmsg "nickserv" . Right $ "IDENTIFY " <> pass
          Nothing   -> return ()

        -- Join channels
        mapM_ (send . Join) chans

-- |Split a network:port string into separate network and port components, defaulting to 6667 if no port is given.
splitNetwork :: Text -> (ByteString, Int)
splitNetwork ns = case readMaybe . unpack . T.drop 1 <$> breakOn ":" ns of
  (network, Just port) -> (pack $ unpack network, port)
  (network, _)         -> (pack $ unpack network, 6667)
