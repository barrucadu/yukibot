{-# LANGUAGE OverloadedStrings #-}

-- |Plugin for dealing with channels.
--
-- TODO: Persist the list of connected channels.
module Yukibot.Plugins.Channels
    ( -- *Commands
      joinCmd
    , partCmd
    ) where

import Data.Text                 (Text)
import Network.IRC.Asakura.Types (Bot)
import Network.IRC.IDTE          (send, join, part, reply)
import Network.IRC.IDTE.Types    (Event(..), IRCState, IRC, Source(..))

-- |Join a channel. The first argument is the name of the channel to
-- join.
joinCmd :: [Text] -> IRCState -> Event -> Bot (IRC ())
joinCmd (chan:_) _ _ = return . send $ join chan
joinCmd _ _ ev = return $ reply ev "Tell me which channel!"

-- |Part a channel.
partCmd :: [Text] -> IRCState -> Event -> Bot (IRC ())
partCmd _ _ ev = return $ case _source ev of
                            Channel _ c -> send . part c $ Just "Goodbye"
                            User _      -> reply ev "This isn't a channel!"
                            _           -> return ()
