{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Yukibot.Plugin.Sen
-- Copyright   : (c) 2016 Michael Walker
-- License     : MIT
-- Stability   : experimental
-- Portability : OverloadedStrings
--
-- Keep track of active users. Provides one monitor and one command:
--
--     * Monitor "seen", log the last thing said by a user.
--
--     * Command "seen", reply with the time and last message a user
--       was seen saying.
module Yukibot.Plugin.Seen where

import Control.Monad.IO.Class (liftIO)
import qualified Data.HashMap.Strict as H
import Data.Maybe (listToMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Database.MongoDB ((=:), at)

import Yukibot.Core

seenPlugin :: config -> Either error Plugin
seenPlugin _ = Right Plugin
  { pluginHelp = "keep track of the last thing said by a user"
  , pluginMonitors = H.fromList [("seen", seenMonitor)]
  , pluginCommands = H.fromList [("seen", seenCommand)]
  }

-- | Record everything said.
seenMonitor :: Monitor
seenMonitor = Monitor
  { monitorHelp = "record everything said"
  , monitorAction = \ev -> case eventChannel ev of
      Just cname -> do
        now <- liftIO getCurrentTime
        upsertMongo [ "user" =: eventUser ev, "channel" =: cname ]
                    [ "user"    =: eventUser ev
                    , "channel" =: cname
                    , "when"    =: now
                    , "message" =: eventMessage ev
                    ]
      Nothing -> pure ()
  }

-- | Get the last thing said by a user.
seenCommand :: Command
seenCommand = Command
  { commandHelp = "get the last thing said by a user"
  , commandAction = \ev args -> case (eventChannel ev, listToMaybe args) of
      (Just cname, Just user) -> do
        let u = UserName user
        seen <- queryMongo [ "user" =: u, "channel" =: cname ] []
        quickReply $ case seen of
          (doc:_) -> formatDoc u (at "when" doc) (at "message" doc)
          [] -> "I haven't seen " <> user <> " say anything yet"
      (Nothing, _) -> reply "The seen command only works inside a channel, silly."
      (_, Nothing) -> reply "You need to name a user, silly."
  }
  where
    -- Format a 'seen' record.
    formatDoc :: UserName -> Maybe UTCTime -> Maybe Text -> Text
    formatDoc (UserName user) (Just when) (Just message) =
      user <> " was last seen at " <>
      T.pack (formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" when) <>
      " saying '" <> message <> "'."
    formatDoc _ _ _ = "Something went wrong :("
