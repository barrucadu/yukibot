{-# LANGUAGE OverloadedStrings     #-}

-- |Respond to predefined phrases.
module Yukibot.Plugins.Trigger
  ( Response(..)
  -- *Event handler
  , eventHandler
  -- Commands
  , addTriggerCmd
  , rmTriggerCmd
  , listTriggerCmd
  -- *Updating
  , addTrigger
  , removeTrigger
  ) where

import Control.Monad (filterM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Monoid ((<>))
import Data.Text (Text, replace, isPrefixOf, isSuffixOf, strip, pack, unpack)
import Database.MongoDB (Document, Selector, (=:), insert_)
import Network.IRC.Bot.Commands (CommandDef(..))
import Network.IRC.Bot.Events (reply, runAlways, runEverywhere)
import Network.IRC.Bot.Types (EventHandler(..), Bot)
import Network.IRC.Client (ctcp, send)
import Network.IRC.Client.Types ( Event(..), EventType(EPrivmsg)
                                , IRC, IRCState
                                , Message(Privmsg)
                                , UnicodeEvent
                                , Source(..))
import System.Random (randomIO, randomRIO)
import Text.Read (readMaybe)
import Text.Regex.TDFA (CompOption(..), defaultCompOpt, defaultExecOpt)
import Text.Regex.TDFA.String (Regex, compile, execute)

import Yukibot.Utils

import qualified Data.Text as T

data Response = TR
  { _response    :: Text
  -- ^The response text. "%n" is replaced with the user's nick
  , _probability :: Double
  -- ^Probability of activating
  }


-- *Event handler

-- |Attempt to match a trigger against an incoming PRIVMSG and respond.
--
-- Triggers are matched by stripping leading and trailing whitespace
-- and by ignoring case.
eventHandler :: EventHandler ()
eventHandler = EventHandler
  { _description = "Respond to messages consisting of trigger phrases."
  , _matchType   = EPrivmsg
  , _eventFunc   = eventFunc
  , _appliesTo   = runEverywhere
  , _appliesDef  = runAlways
  }

eventFunc :: IRCState () -> UnicodeEvent -> Bot (IRC ())
eventFunc _ ev = do
  mongo    <- defaultMongo "triggers"
  triggers <- queryMongo mongo (triggerSel ev) []

  return $ do
    let Privmsg _ (Right msg) = _message ev
    respond ev $ findTriggers (T.strip msg) triggers

-- Find the triggers matching this message.
findTriggers :: Text -> [Document] -> [Response]
findTriggers target = mapMaybe findTrigger' where
  findTrigger' trig =
    let trig' = at' "trigger" "" trig
    in case tryRegex trig' of
        Just r  | r =~ target -> Just $ extract trig
                | otherwise -> Nothing
        Nothing | T.toLower trig' == T.toLower target -> Just $ extract trig
                | otherwise -> Nothing

  r =~ txt = case execute r (T.unpack txt) of
    Right (Just _) -> True
    _ -> False

  extract trig = TR (at' "response" "I am so triggered right now." trig) (at' "probability" 1 trig)

-- |Pick a random response and send it
respond :: UnicodeEvent -> [Response] -> IRC ()
respond ev rs = do
  responses <- liftIO $ filterM (\r -> (<= _probability r) <$> randomIO) rs

  case responses of
    [] -> return ()
    _ -> do
      idx <- liftIO $ randomRIO (0, length responses - 1)
      reply' . replace "%channel" chan . replace "%nick" nick $ _response (responses !! idx)

  where
    chan = case _source ev of
      Channel c _ -> c
      _ -> ""

    nick = case _source ev of
      User n      -> n
      Channel _ n -> n
      _ -> ""

    reply' resp | "/me" `isPrefixOf` resp = send $ ctcp (if T.null chan then nick else chan) "ACTION" [T.strip $ T.drop 3 resp]
                | otherwise = reply ev resp

-- *Commands

-- |Add a new trigger
--
-- Because triggers can be arbitrary text, " <reply> " is used to
-- delimit the trigger and the response.
addTriggerCmd :: CommandDef ()
addTriggerCmd = CommandDef
  { _verb = ["add", "trigger"]
  , _help = "<trigger> \\<reply\\> <response> -- add a trigger, as triggers can be arbitrary text, '<reply>' is used to delimit the trigger and the response. Regex triggers start and end with '/'. \\<reply <num>\\> can be used to set the probability (0 to 1)."
  , _action = go
  }

  where
    go vs _ ev = case () of
      _ | "<reply>" `elem` vs ->
          case break (=="<reply>") vs of
            (pref, "<reply>":suf) -> go' (T.unwords pref) (T.unwords suf) 1 ev
            _ -> return $ return ()

        | "<reply" `elem` vs ->
          case break (=="<reply") vs of
            (pref, "<reply":prob:suf) -> go' (T.unwords pref) (T.unwords suf) (fromMaybe 1 . readMaybe . filter (/='>') . unpack $ prob) ev
            _ -> return $ return ()

        | otherwise -> return $ return ()

    go' trig resp prob ev = case _source ev of
      Channel c _ ->
        let trig' = strip trig
            resp' = TR
              { _response    = strip resp
              , _probability = if prob > 1 || prob < 0 then 1 else prob
              }
        in do
          mongo <- defaultMongo "triggers"
          addTrigger mongo trig' resp' c
          return $ return ()
      _ -> return $ reply ev "Can only add triggers in a channel."

-- Remove a trigger
rmTriggerCmd :: CommandDef ()
rmTriggerCmd = CommandDef
  { _verb = ["remove", "trigger"]
  , _help = "<trigger> - remove the named trigger"
  , _action = go
  }

  where
    go vs _ ev = case _source ev of
      Channel c _ -> do
        mongo <- defaultMongo "triggers"
        removeTrigger mongo (strip $ T.unwords vs) c
        return $ return ()
      _ -> return $ reply ev "Can only remove triggers in a channel."

-- |List all triggers in this channel, by uploading to sprunge
listTriggerCmd :: CommandDef ()
listTriggerCmd = CommandDef
  { _verb = ["list", "triggers"]
  , _help = "List all the current triggers for this channel"
  , _action = go
  }

  where
    go _ _ ev = do
      mongo <- defaultMongo "triggers"
      trigs <- allTriggers mongo (triggerSel ev)
      uri <- paste trigs
      return $ reply ev uri

    -- Get all triggers, as a newline-delimited string
    allTriggers mongo selector = unlines . map ppTrig <$> queryMongo mongo selector ["trigger" =: (1 :: Int)]

    -- Pretty-print an individual trigger
    ppTrig trig = T.unpack $ at' "trigger" "" trig <> ppProb trig <> at' "response" "I am so triggered right now." trig
    ppProb trig = let prob = at' "probability" (1::Float) trig in if prob == 1 then " <reply> " else " <reply " <> pack (show prob) <> "> "

-- *Updating

-- |Add a new trigger
addTrigger :: MonadIO m => Mongo -> Text -> Response -> Text -> m ()
addTrigger mongo trig resp channel = doMongo mongo add where
  add c = insert_ c ["trigger" =: trig, "response" =: _response resp, "probability" =: _probability resp, "channel" =: channel]

-- |Remove a trigger
removeTrigger :: MonadIO m => Mongo -> Text -> Text -> m ()
removeTrigger mongo trig channel = deleteMongo mongo ["trigger" =: trig, "channel" =: channel]

-- *Utils

-- Attempt to interpret the trigger as regex, and compile it.
tryRegex :: Text -> Maybe Regex
tryRegex trig
  | "/" `isPrefixOf` trig && "/" `isSuffixOf` trig && T.length trig >= 2 = doRegex'
  | otherwise = Nothing

  where
    -- Strip the leading and trailing '/', and attempt to compile the inner regex.
    doRegex' = either (const Nothing) Just . compile copt eopt . T.unpack . T.init . T.tail $ trig

    copt = defaultCompOpt { caseSensitive = False }
    eopt = defaultExecOpt

-- | Get the selector for triggers in the current channel.
triggerSel :: UnicodeEvent -> Selector
triggerSel ev = case _source ev of
  Channel c _ -> ["channel" =: c]
  _ -> []
