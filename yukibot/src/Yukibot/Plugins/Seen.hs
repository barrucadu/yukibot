{-# LANGUAGE OverloadedStrings #-}

-- |Keep track of the last time we saw someone
module Yukibot.Plugins.Seen
    ( -- *Event handler
      eventHandler
      -- *Command
    , command
    ) where

import Control.Applicative        ((<$>))
import Data.Monoid                ((<>))
import Data.Text                  (Text)
import Network.IRC.Asakura.Events (runEverywhere)
import Network.IRC.Asakura.Types  (AsakuraEventHandler(..), Bot)
import Network.IRC.IDTE           (reply)
import Network.IRC.IDTE.Types     ( ConnectionConfig(_server)
                                  , Event(..), EventType(EPrivmsg)
                                  , IRC, IRCState
                                  , Message(..)
                                  , Source(Channel)
                                  , UnicodeEvent
                                  , connectionConfig)
import Yukibot.Plugins.Memory     (MemoryState, getFactValue, setFactValues)

-- *Event handler

eventHandler :: MemoryState -> AsakuraEventHandler
eventHandler ms = AsakuraEventHandler
                    { _description = "Keep track of the last thing a person said"
                    , _matchType   = EPrivmsg
                    , _eventFunc   = eventFunc ms
                    , _appliesTo   = runEverywhere
                    , _appliesDef  = const $ return False
                    }

eventFunc :: MemoryState -> IRCState -> UnicodeEvent -> Bot (IRC ())
eventFunc ms _ ev = return $ do
  let Channel channel nick  = _source ev
  let Privmsg _ (Right msg) = _message ev

  network <- _server <$> connectionConfig

  setFactValues ms network nick ("seen-" <> channel) [msg]

-- *Command

command :: MemoryState -> [Text] -> IRCState -> UnicodeEvent -> Bot (IRC ())
command ms (nick:_) _ ev = return $ do
  let channel = case _source ev of
                  Channel c _ -> Just c
                  _           -> Nothing

  network <- _server <$> connectionConfig

  case channel of
    Just chan -> do
      val <- getFactValue ms network nick ("seen-" <> chan)
      case val of
        Just msg -> reply ev $ nick <> " was last seen saying: " <> msg
        Nothing  -> reply ev $ "I haven't seen " <> nick <> " say anything yet."

    Nothing -> return ()

command _ [] _ ev = return $ reply ev "You need to give me a nick."
