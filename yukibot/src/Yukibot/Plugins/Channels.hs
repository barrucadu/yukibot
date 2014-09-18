
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
import Network.IRC.Asakura.Commands (CommandDef(..), CommandState, setChannelPrefix, unsetChannelPrefix)
import Network.IRC.Client           (send, reply)
import Network.IRC.Client.Types     (ConnectionConfig(_server), Event(..), Message(..), Source(..), connectionConfig)

-- |Join a channel. The first argument is the name of the channel to
-- join.
joinCmd :: CommandDef
joinCmd = CommandDef ["join"] go
  where
    go (chan:_) _ _ = return . send $ Join chan
    go _ _ ev = return $ reply ev "Tell me which channel!"

-- |Part a channel.
partCmd :: CommandDef
partCmd = CommandDef ["part"] go
  where
    go _ _ ev = return $
      case _source ev of
        Channel c _ -> send . Part c $ Just "Goodbye"
        User _      -> reply ev "This isn't a channel!"
        _           -> return ()

-- |Set the channel-specific prefix
setChanPrefix :: CommandState -> CommandDef
setChanPrefix = CommandDef ["set", "prefix"] . go
  where
    go cs (pref:_) _ ev = return $ do
      network <- _server <$> connectionConfig
      case _source ev of
        Channel c _ -> setChannelPrefix cs network c pref
        _           -> reply ev "This isn't a channel!"
    go _ [] _ ev = return . reply ev $ "You need to give me a new prefix!"

-- |Unset the channel-specific prefix
unsetChanPrefix :: CommandState -> CommandDef
unsetChanPrefix = CommandDef ["unset", "prefix"] . go
  where
    go cs _ _ ev = return $ do
      network <- _server <$> connectionConfig
      case _source ev of
        Channel c _ -> unsetChannelPrefix cs network c
        _           -> reply ev "This isn't a channel!"
