{-# LANGUAGE OverloadedStrings #-}

-- |Plugin for dealing with channels.
--
-- TODO: Persist the list of connected channels.
module Yukibot.Plugins.Channels
    ( -- *Commands
      joinCmd
    , partCmd
    ) where

import Data.Monoid               ((<>))
import Data.Text                 (Text)
import Network.IRC.Asakura.Types (Bot)
import Network.IRC.IDTE          (send, join, part, privmsg, query)
import Network.IRC.IDTE.Types    (Event(..), IRCState, IRC, Source(..))

-- |Join a channel. The first argument is the name of the channel to
-- join.
joinCmd :: [Text] -> IRCState -> Event -> Bot (IRC ())
joinCmd (chan:_) _ _ = return . send $ join chan
joinCmd _ _ ev = return $ case _source ev of
                            Channel n c -> send . privmsg c $ n <> ": " <> "Tell me which channel!"
                            User n      -> send $ query n "Tell me which channel!"
                            _           -> return ()

-- |Part a channel.
partCmd :: [Text] -> IRCState -> Event -> Bot (IRC ())
partCmd _ _ ev = return $ case _source ev of
                            Channel _ c -> send . part c $ Just "Goodbye"
                            User n      -> send $ query n "This isn't a channel!"
                            _           -> return ()
