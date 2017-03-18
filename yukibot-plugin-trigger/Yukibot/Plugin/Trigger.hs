{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Yukibot.Plugin.Trigger
-- Copyright   : (c) 2016 Michael Walker
-- License     : MIT
-- Stability   : experimental
-- Portability : OverloadedStrings
--
-- Match messages and respond to them! Triggers are channel-scoped,
-- and expansion text can reference the triggering event.
--
-- Provides one monitor:
--
--     * "trigger", match messages against known triggers and respond
--       with a random match.
--
-- And four commands:
--
--     * "new", define a new trigger, requires the user to be a deity.
--
--     * "delete", delete a trigger, requires the user to be a deity.
--
--     * "clear", delete all triggers for this channel, requires the
--       user to be a deity.
--
--     * "list", upload the triggers for this channel and respond with
--       a link.
module Yukibot.Plugin.Trigger where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import qualified Data.HashMap.Strict as H
import Data.Maybe (fromJust, fromMaybe, mapMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Database.MongoDB ((=:), at)
import Numeric (showFFloat)
import System.Random (randomRIO)
import Text.Read (readMaybe)
import Text.Regex.TDFA (CompOption(..), defaultCompOpt, defaultExecOpt)
import Text.Regex.TDFA.String (Regex, compile, execute)

import Yukibot.Core
import Yukibot.Utils

data Response = Response
  { responseText   :: Text
  , responseDirect :: Bool
  , responseProb   :: Double
  }
  deriving (Read, Show)

-------------------------------------------------------------------------------
-- * Plugin

triggerPlugin :: config -> Either error Plugin
triggerPlugin _ = Right Plugin
  { pluginHelp = "match messages and automatically respond to them"
  , pluginMonitors = H.fromList [("trigger", triggerMonitor)]
  , pluginCommands = H.fromList
    [ ("new",    newCommand)
    , ("delete", deleteCommand)
    , ("clear",  clearCommand)
    , ("list",   listCommand)
    ]
  }

-------------------------------------------------------------------------------
-- * Monitors

-- | Match messages against known triggers and respond with a random match.
triggerMonitor :: Monitor
triggerMonitor = Monitor
  { monitorHelp   = "match messages against known triggers and respond with a random match"
  , monitorAction = \ev -> case eventChannel ev of
      Just cname -> do
        -- Responses from all matching triggers in this channel.
        allResponses <- mapMaybe (match ev) <$> queryMongo ["channel" =: cname] []

        -- Filter by probability
        prob <- liftIO $ randomRIO (0, 1)
        let rs = filter (\r -> prob <= responseProb r) allResponses

        -- Pick a random trigger and respond.
        unless (null rs) $ do
          idx <- liftIO $ randomRIO (0, length rs - 1)
          let r = rs !! idx
          (if responseDirect r then reply else quickReply) (responseText r)
      Nothing -> pure ()
  }

  where
    -- Match a trigger against the event, and substitute in values.
    match ev doc
      | Just direct      <- at "direct"      doc
      , Just probability <- at "probability" doc
      , Just trigger     <- at "trigger"     doc
      , Just response    <- at "response"    doc
      =
      let msg  = T.toLower (eventMessage ev)
          trig = T.toLower trigger
          resp = Response (substitute ev response) direct probability
      in case tryRegex trigger of
           Just r  | r =~ msg    -> Just resp
                   | otherwise   -> Nothing
           Nothing | trig == msg -> Just resp
                   | otherwise   -> Nothing
    match _ _ = Nothing

    -- Substitute variables into a response.
    substitute ev = T.replace "%message" (eventMessage ev)
                  . T.replace "%whoami"  (getUserName $ eventWhoAmI ev)
                  . T.replace "%user"    (getUserName $ eventUser   ev)
                  . T.replace "%channel" (getChannelName . fromJust $ eventChannel ev)

-------------------------------------------------------------------------------
-- * Commands

-- | Define a new trigger.
newCommand :: Command
newCommand = privilegedCommand Command
  { commandHelp   = "\"regex or words <[!]reply[ prob]> words\": define a new trigger"
  , commandAction = \ev args -> case eventChannel ev of
      Just cname -> case findTrigger args of
        Just (direct, probability, trigger, response) ->
          insertMongo [[ "channel"     =: cname
                       , "direct"      =: direct
                       , "probability" =: probability
                       , "trigger"     =: trigger
                       , "response"    =: response
                       ]]
        Nothing -> reply "I didn't quite get that."
      Nothing -> reply "Triggers only work in channels."
  }
  where
    -- Syntax: "trigger <reply> response"
    --       | "trigger <!reply> response"
    --       | "trigger <reply prob> response"
    --       | "trigger <!reply prob> response"
    --
    -- A "!" means that this is a direct reply.
    findTrigger xs
      | "<reply>" `elem` xs = case break (=="<reply>") xs of
          (pref, "<reply>":suf) -> mk False "1" pref suf
          _ -> Nothing
      | "<!reply>" `elem` xs = case break (=="<!reply>") xs of
          (pref, "<!reply>":suf) -> mk True "1" pref suf
          _ -> Nothing
      | "<reply" `elem` xs = case break (=="<reply") xs of
          (pref, "<reply":prob:suf) -> mk False prob pref suf
          _ -> Nothing
      | "<!reply" `elem` xs = case break (=="<!reply") xs of
          (pref, "<!reply":prob:suf) -> mk True prob pref suf
          _ -> Nothing
      | otherwise = Nothing

    -- Helper for 'findTrigger'
    mk direct prob pref suf = Just ( direct
                                   , fromMaybe (1::Float) . readMaybe . filter (/='>') . T.unpack $ prob
                                   , T.unwords pref
                                   , T.unwords suf
                                   )

-- | Delete a trigger.
deleteCommand :: Command
deleteCommand = privilegedCommand Command
  { commandHelp   = "\"trigger text\": delete a trigger"
  , commandAction = \ev args -> case eventChannel ev of
      Just cname -> deleteMongo ["trigger" =: T.unwords args, "channel" =: cname]
      Nothing -> reply "Triggers only work in channels."
  }

-- | Delete all triggers.
clearCommand :: Command
clearCommand = privilegedCommand Command
  { commandHelp   = "delete all triggers"
  , commandAction = \ev _ -> case eventChannel ev of
      Just cname -> deleteMongo ["channel" =: cname]
      Nothing -> reply "Triggers only work in channels."
  }

-- | List triggers.
listCommand :: Command
listCommand = Command
  { commandHelp   = "list triggers"
  , commandAction = \ev _ -> case eventChannel ev of
      Just cname -> do
        triggers <- queryMongo ["channel" =: cname] ["trigger" =: (1 :: Int)]
        muri <- liftIO . paste . unlines $ mapMaybe pprint triggers
        case muri of
          Just uri -> reply $ "See " <> T.pack uri <> " for my triggers."
          Nothing  -> reply "sprunge.us is triggering me right now."
      Nothing -> reply "Triggers only work in channels."
  }
  where
    -- Pretty-print a trigger
    pprint doc
      | Just direct      <- at "direct"      doc
      , Just probability <- at "probability" doc
      , Just trigger     <- at "trigger"     doc
      , Just response    <- at "response"    doc
      = Just . T.unpack $ case (direct, probability) of
          (True,  1) -> trigger <> " <!reply> " <> response
          (False, 1) -> trigger <> " <reply> "  <> response
          (True,  p) -> trigger <> " <!reply "  <> showFloat p <> "> " <> response
          (False, p) -> trigger <> " <reply "   <> showFloat p <> "> " <> response
    pprint _ = Nothing

    -- Show a 'Float' to two decimal places.
    showFloat :: Float -> Text
    showFloat x = T.pack $ showFFloat (Just 2) x ""

-------------------------------------------------------------------------------
-- * Regex helpers

-- | Match a regex against some text.
(=~) :: Regex -> Text -> Bool
r =~ txt = case execute r (T.unpack txt) of
  Right (Just _) -> True
  _ -> False

-- | Try to compile a string in the form \"/regex/\" to a
-- case-insensitive regular expression.
tryRegex :: Text -> Maybe Regex
tryRegex r
  | "/" `T.isPrefixOf` r && "/" `T.isSuffixOf` r && T.length r >= 2 = toRegex (T.init . T.tail $ r)
  | otherwise = Nothing
  where
    toRegex = either (const Nothing) Just . compile copt eopt . T.unpack
    copt = defaultCompOpt { caseSensitive = False }
    eopt = defaultExecOpt
