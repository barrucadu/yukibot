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
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Database.MongoDB (Selector, (=:), at)

import Yukibot.Core
import Yukibot.Extra

seenPlugin :: config -> Either error Plugin
seenPlugin _ = Right Plugin
  { pluginHelp = "keep track of the last thing said by a user"
  , pluginMonitors = H.fromList [("seen", seenMonitor)]
  , pluginCommands = H.fromList [ ("seen", seenCommand)
                                , ("quote-add",    quoteAddCommand)
                                , ("quote-list",   quoteListCommand)
                                , ("quote-remove", quoteRemoveCommand)
                                ]
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
      showTime when <>
      " UTC saying '" <> message <> "'."
    formatDoc _ _ _ = "Something went wrong :("

-- | Add a new quote.
quoteAddCommand :: Command
quoteAddCommand = Command
  { commandHelp   = "add a new quote"
  , commandAction = \ev args -> case eventChannel ev of
      Just cname -> case args of
        [who] -> do
          seen <- listToMaybe <$> queryMongo [ "user" =: UserName who, "channel" =: cname ][]
          case (,) <$> (seen >>= at "when") <*> (seen >>= at "message") of
            Just (when, message) ->
              addQuote cname (when::UTCTime) who (message::Text)
            Nothing ->
              reply $ "I haven't seen " <> who <> " say anything yet"
        (who:what) -> do
          now <- liftIO getCurrentTime
          addQuote cname now who (T.unwords what)
        _ -> reply "Who do you want me to quote?"
      Nothing -> reply "Quotes only work in channels."
  }
  where
    addQuote cname when who what = insertMongo
      [[ "channel" =: cname
       , "when"    =: when
       , "quotee"  =: who
       , "quote"   =: what
       ]]

-- | List all quotes in the current channel.
quoteListCommand :: Command
quoteListCommand = Command
  { commandHelp   = "list quotes"
  , commandAction = \ev args -> case eventChannel ev of
      Just cname -> do
        quotes <- findQuotes cname args
        case quotes of
          []  -> reply "There are no quotes."
          [q] -> reply q
          _   -> do
            muri <- liftIO . paste . T.unpack $ T.unlines quotes
            case muri of
              Just uri -> reply $ "See " <> T.pack uri <> " for the quotes."
              Nothing  -> reply "sprunge.us is not working right now."
      Nothing -> reply "Quotes only work in channels."
  }

-- | Delete quotes in the current channel.
quoteRemoveCommand :: Command
quoteRemoveCommand = Command
  { commandHelp   = "delete quotes"
  , commandAction = \ev args -> case eventChannel ev of
      Just cname
        | null args -> reply "You need to give a query."
        | otherwise -> do
          quotes <- findQuotes cname args
          muri   <- liftIO . paste . T.unpack $ T.unlines quotes
          case (quotes, muri) of
            ([], _)      -> reply "There are no quotes matching that query."
            (_, Nothing) -> reply "sprunge.us is not working right now, so I'm not deleting those quotes."
            (_, Just uri) -> do
              reply $ "See " <> T.pack uri <> " for the deleted quotes."
              deleteMongo (qfilter cname args)
      Nothing -> reply "Quotes only work in channels."
  }

-- | Find and pretty-print quotes.
findQuotes :: ChannelName -> [Text] -> BackendM [Text]
findQuotes cname args = mapMaybe pprint <$> queryMongo (qfilter cname args) ["when" =: (1 :: Int)] where
  pprint doc
    | Just when   <- at "when"   doc
    , Just quotee <- at "quotee" doc
    , Just quote  <- at "quote"  doc
    = Just $ showTime when <> " <" <> quotee <> "> " <> quote
  pprint _ = Nothing

-- | Selector for quotes in a channel.
qfilter :: ChannelName -> [Text] -> Selector
qfilter cname args =
  [ "channel" =: cname
  , "quotee"  =: ["$exists" =: True]
  , "quote"   =: ["$exists" =: True]
  , "$or" =: [ ["quotee" =: ("$regex" =: T.intercalate ".*" args)]
             , ["quote"  =: ("$regex" =: T.intercalate ".*" args)]
             ]
  ]

-- | Format a timestamp.
showTime :: UTCTime -> Text
showTime = T.pack . formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S"
