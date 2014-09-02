
{-# LANGUAGE OverloadedStrings #-}

-- |Plugin for dealing with channels.
--
-- TODO: Persist the list of connected channels.
module Yukibot.Plugins.Channels
    ( -- *Commands
      joinCmd
    , partCmd
    , setChanPrefix
    , unsetChanPrefix
    ) where

import Control.Applicative          ((<$>))
import Data.Text                    (Text)
import Network.IRC.Asakura.Commands (CommandState, setChannelPrefix, unsetChannelPrefix)
import Network.IRC.Asakura.Types    (Bot)
import Network.IRC.IDTE             (send, reply)
import Network.IRC.IDTE.Types       (ConnectionConfig(_server), Event(..), Message(..), IRCState, IRC, Source(..), UnicodeEvent, connectionConfig)

-- |Join a channel. The first argument is the name of the channel to
-- join.
joinCmd :: [Text] -> IRCState -> UnicodeEvent -> Bot (IRC ())
joinCmd (chan:_) _ _ = return . send $ Join chan
joinCmd _ _ ev = return $ reply ev "Tell me which channel!"

-- |Part a channel.
partCmd :: [Text] -> IRCState -> UnicodeEvent -> Bot (IRC ())
partCmd _ _ ev = return $ case _source ev of
                            Channel c _ -> send . Part c $ Just "Goodbye"
                            User _      -> reply ev "This isn't a channel!"
                            _           -> return ()

-- |Set the channel-specific prefix
setChanPrefix :: CommandState -> [Text] -> IRCState -> UnicodeEvent -> Bot (IRC ())
setChanPrefix cs (pref:_) _ ev = return $ do
  network <- _server <$> connectionConfig
  case _source ev of
    Channel c _ -> setChannelPrefix cs network c pref
    _           -> reply ev "This isn't a channel!"
setChanPrefix _ [] _ ev = return . reply ev $ "You need to give me a new prefix!"

-- |Unset the channel-specific prefix
unsetChanPrefix :: CommandState -> [Text] -> IRCState -> UnicodeEvent -> Bot (IRC ())
unsetChanPrefix cs _ _ ev = return $ do
  network <- _server <$> connectionConfig
  case _source ev of
    Channel c _ -> unsetChannelPrefix cs network c
    _           -> reply ev "This isn't a channel!"
