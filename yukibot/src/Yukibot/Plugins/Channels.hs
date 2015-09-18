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
  -- *Events
  , inviteEv
  ) where

import Network.IRC.Bot.Commands
import Network.IRC.Bot.Events
import Network.IRC.Bot.Types
import Network.IRC.Client (Event(..), EventType(..), Message(..), Source(..), send)
import Network.IRC.Client.Types (ConnectionConfig(..), connectionConfig)

-- |Join a channel. The first argument is the name of the channel to
-- join.
joinCmd :: CommandDef
joinCmd = CommandDef
  { _verb   = ["join"]
  , _help   = "Join a named channel."
  , _action = go
  }

  where
    go (chan:_) _ _ = return . send $ Join chan
    go _ _ ev = return $ reply ev "Tell me which channel!"

-- |Part a channel.
partCmd :: CommandDef
partCmd = CommandDef
  { _verb   = ["part"]
  , _help   = "Leave the current channel."
  , _action = go
  }

  where
    go _ _ ev = return $
      case _source ev of
        Channel c _ -> send . Part c $ Just "Goodbye"
        User _      -> reply ev "This isn't a channel!"
        _           -> return ()

-- |Set the channel-specific prefix
setChanPrefix :: CommandState -> CommandDef
setChanPrefix cs = CommandDef
  { _verb   = ["set", "prefix"]
  , _help   = "Set the command prefix for this channel."
  , _action = go
  }

  where
    go (pref:_) _ ev = return $ do
      network <- _server <$> connectionConfig
      case _source ev of
        Channel c _ -> setChannelPrefix cs network c pref
        _           -> reply ev "This isn't a channel!"
    go [] _ ev = return . reply ev $ "You need to give me a new prefix!"

-- |Unset the channel-specific prefix
unsetChanPrefix :: CommandState -> CommandDef
unsetChanPrefix cs = CommandDef
  { _verb   = ["unset", "prefix"]
  , _help   = "Remove the command prefix for this channel."
  , _action = go
  }

  where
    go _ _ ev = return $ do
      network <- _server <$> connectionConfig
      case _source ev of
        Channel c _ -> unsetChannelPrefix cs network c
        _           -> reply ev "This isn't a channel!"

-- | Respond to INVITE.
inviteEv :: EventHandler
inviteEv = EventHandler
  { _description = "Join channels INVITEd to."
  , _matchType   = EInvite
  , _eventFunc   = go
  , _appliesTo   = runEverywhere
  , _appliesDef  = runAlways
  }

  where
    go _ ev = case _message ev of
      Invite chan _ -> return . send $ Join chan
