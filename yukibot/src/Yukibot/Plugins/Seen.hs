{-# LANGUAGE OverloadedStrings #-}

-- |Keep track of the last time we saw someone
module Yukibot.Plugins.Seen
  ( -- *Event handler
    eventHandler
    -- *Command
  , command
  ) where

import Data.Monoid ((<>))
import Network.IRC.Bot.Commands (CommandDef(..))
import Network.IRC.Bot.Events (reply, runEverywhere)
import Network.IRC.Bot.Types (EventHandler(..), Bot)
import Network.IRC.Client.Types ( ConnectionConfig(_server)
                                , Event(..), EventType(EPrivmsg)
                                , IRC, IRCState
                                , Message(..)
                                , Source(Channel)
                                , UnicodeEvent
                                , connectionConfig)

import Yukibot.Plugins.Memory
import Yukibot.Utils

-- *Event handler

eventHandler :: EventHandler ()
eventHandler = EventHandler
  { _description = "Keep track of the last thing a person said"
  , _matchType   = EPrivmsg
  , _eventFunc   = eventFunc
  , _appliesTo   = runEverywhere
  , _appliesDef  = const $ return False
  }

eventFunc :: IRCState () -> UnicodeEvent -> Bot (IRC ())
eventFunc _ ev = do
  let Channel channel nick  = _source ev
  let Privmsg _ (Right msg) = _message ev

  ms <- defaultMongo "seen"

  return $ do
    network <- _server <$> connectionConfig
    setFactValues ms network nick channel [msg]

-- *Command

command :: CommandDef ()
command = CommandDef
  { _verb   = ["seen"]
  , _help = "<nick> - Get the last thing said by that nick in this channel."
  , _action = go
  }

  where
    go (nick:_) _ ev = do
      ms <- defaultMongo "seen"

      return $ do
        let channel = case _source ev of
                        Channel c _ -> Just c
                        _           -> Nothing

        network <- _server <$> connectionConfig

        case channel of
          Just chan -> do
            val <- getFactValue ms network nick chan
            case val of
              Just (msg, utc) -> reply ev $ nick <> " was last seen saying: \"" <> msg <> "\" at " <> showUtc utc
              Nothing -> reply ev $ "I haven't seen " <> nick <> " say anything yet."

          Nothing -> return ()

    go [] _ ev = return $ reply ev "You need to give me a nick."
