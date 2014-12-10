{-# LANGUAGE MultiParamTypeClasses #-}
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

import Control.Applicative        ((<$>))
import Control.Monad              (when)
import Control.Monad.IO.Class     (MonadIO, liftIO)
import Data.Maybe                 (listToMaybe, mapMaybe)
import Data.Monoid                ((<>))
import Data.Text                  (Text, replace, isPrefixOf, isSuffixOf, breakOn, strip)
import Database.MongoDB           (Document, (=:), insert_)
import Network.IRC.Asakura.Commands (CommandDef(..))
import Network.IRC.Asakura.Events (runAlways, runEverywhere)
import Network.IRC.Asakura.Types  (AsakuraEventHandler(..), Bot)
import Network.IRC.Client         (ctcp, reply, send)
import Network.IRC.Client.Types   ( Event(..), EventType(EPrivmsg)
                                  , IRC, IRCState
                                  , Message(Privmsg)
                                  , UnicodeEvent
                                  , Source(..))
import System.Random              (randomIO)
import Text.Regex.TDFA            (CompOption(..), defaultCompOpt, defaultExecOpt)
import Text.Regex.TDFA.String     (Regex, compile, execute)
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
eventHandler :: AsakuraEventHandler
eventHandler = AsakuraEventHandler
  { _description = "Respond to messages consisting of trigger phrases."
  , _matchType   = EPrivmsg
  , _eventFunc   = eventFunc
  , _appliesTo   = runEverywhere
  , _appliesDef  = runAlways
  }

eventFunc :: IRCState -> UnicodeEvent -> Bot (IRC ())
eventFunc _ ev = do
  mongo    <- defaultMongo "triggers"
  triggers <- queryMongo mongo [] []

  return $ do
    let Privmsg _ (Right msg) = _message ev

    case findTrigger (T.strip msg) triggers of
      Just resp -> respond ev resp
      Nothing   -> return ()

-- Find the first trigger matching this message.
findTrigger :: Text -> [Document] -> Maybe Response
findTrigger target = listToMaybe . mapMaybe findTrigger'
  where
    findTrigger' trig =
      let trig' = at' "trigger" "" trig
      in case tryRegex trig' of
          Just r  -> if r =~ target                        then Just $ extract trig else Nothing
          Nothing -> if T.toLower trig' == T.toLower target then Just $ extract trig else Nothing

    r =~ txt =
      case execute r (T.unpack txt) of
        Right (Just _) -> True
        _ -> False

    extract trig = TR (at' "response" "I am so triggered right now." trig) (at' "probability" 1 trig)

respond :: UnicodeEvent -> Response -> IRC ()
respond ev r = do
  roll <- liftIO randomIO

  when (roll <= _probability r) $
     reply' . replace "%channel" chan . replace "%nick" nick . _response $ r

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
addTriggerCmd :: CommandDef
addTriggerCmd = CommandDef
  { _verb = ["add", "trigger"]
  , _help = "<trigger> \\<reply\\> <response> -- add a trigger, as triggers can be arbitrary text, '<reply>' is used to delimit the trigger and the response. Regex triggers start and end with '/'."
  , _action = go
  }

  where
    go vs _ _ = go' $ T.unwords vs
    go' trig = go'' $ T.drop 7 <$> breakOn "<reply>" trig
    go'' (trig, resp) =
      let trig' = strip trig
          resp' = TR
            { _response    = strip resp
            , _probability = 1
            }
      in do
        mongo <- defaultMongo "triggers"
        addTrigger mongo trig' resp'
        return $ return ()

-- Remove a trigger
rmTriggerCmd :: CommandDef
rmTriggerCmd = CommandDef
  { _verb = ["remove", "trigger"]
  , _help = "<trigger> - remove the named trigger"
  , _action = go
  }

  where
    go vs _ _ = do
      mongo <- defaultMongo "triggers"
      removeTrigger mongo . strip $ T.unwords vs
      return $ return ()

-- |List all triggers, by uploading to sprunge
listTriggerCmd :: CommandDef
listTriggerCmd = CommandDef
  { _verb = ["list", "triggers"]
  , _help = "List all the current triggers"
  , _action = go
  }

  where
    go _ _ ev = do
      mongo <- defaultMongo "triggers"
      trigs <- allTriggers mongo
      uri <- paste trigs
      return $ reply ev uri

    -- Get all triggers, as a newline-delimited string
    allTriggers mongo = unlines . map ppTrig <$> queryMongo mongo [] ["trigger" =: (1 :: Int)]

    -- Pretty-print an individual trigger
    ppTrig trig = T.unpack $ at' "trigger" "" trig <> " <reply> " <> at' "response" "I am so triggered right now." trig

-- *Updating

-- |Add a new trigger
addTrigger :: MonadIO m => Mongo -> Text -> Response -> m ()
addTrigger mongo trig resp = doMongo mongo add
  where
    add c = insert_ c ["trigger" =: trig, "response" =: _response resp, "probability" =: _probability resp]

-- |Remove a trigger
removeTrigger :: MonadIO m => Mongo -> Text -> m ()
removeTrigger mongo trig = deleteMongo mongo ["trigger" =: T.toLower trig]

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
