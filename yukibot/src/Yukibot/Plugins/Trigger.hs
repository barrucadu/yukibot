{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

-- |Respond to predefined phrases.
module Yukibot.Plugins.Trigger
    ( -- *State
      TriggerState
    , Response(..)
    -- *Snapshotting
    , TriggerStateSnapshot
    -- *Event handler
    , eventHandler
    -- *Updating
    , addTrigger
    , removeTrigger
    , blacklist
    , whitelist
    ) where

import Control.Applicative        ((<$>), (<*>))
import Control.Concurrent.STM     (TVar, atomically, newTVar, readTVar, writeTVar)
import Control.Lens               ((&), (^.), (.~), (%~), at, non)
import Control.Monad              (liftM, when)
import Control.Monad.IO.Class     (MonadIO, liftIO)
import Data.Aeson                 (FromJSON(..), ToJSON(..), Value(..), (.=), (.:), (.:?), (.!=), object)
import Data.ByteString            (ByteString)
import Data.ByteString.Char8      (unpack)
import Data.Default.Class         (Default(..))
import Data.Map                   (Map, mapWithKey, foldWithKey)
import Data.Maybe                 (fromMaybe)
import Data.Text                  (Text, replace, isPrefixOf, isSuffixOf)
import Network.IRC.Asakura.Events (runAlways, runEverywhere)
import Network.IRC.Asakura.Types  (AsakuraEventHandler(..), Bot)
import Network.IRC.Asakura.State  (Snapshot(..), Rollback(..))
import Network.IRC.Client         (ctcp, reply, send)
import Network.IRC.Client.Types   ( ConnectionConfig(..)
                                  , Event(..), EventType(EPrivmsg)
                                  , IRC, IRCState
                                  , Message(Privmsg)
                                  , UnicodeEvent
                                  , Source(..)
                                  , connectionConfig)
import System.Random              (randomIO)
import Text.Regex.TDFA            (CompOption(..), defaultCompOpt, defaultExecOpt)
import Text.Regex.TDFA.String     (Regex, compile, execute)

import qualified Data.Map  as M
import qualified Data.Text as T

-- *State

-- |List of all triggers, along with a network/channel blacklist.
newtype TriggerState = TS { _triggers :: TVar (Map Text Response) }

data Response = TR
    { _response    :: Text
    -- ^The response text. "%n" is replaced with the user's nick
    , _blacklist   :: Map String [Text]
    -- ^Servers/channels for which this trigger is inactive
    , _probability :: Double
    -- ^Probability of activating
    , _regex       :: Maybe Regex
    -- ^If the trigger is a regular expression string, this is the
    -- compiled regex.
    }

instance FromJSON Response where
    parseJSON (Object v) = tr <$> v .:  "response"
                              <*> v .:? "blacklist"   .!= M.empty
                              <*> v .:? "probability" .!= 1
      where
        tr r b p = TR r b p Nothing
    parseJSON _ = fail "Expected object"

instance ToJSON Response where
    toJSON r = object $ [ "response" .= _response r] ++ blackl ++ prob

        where blackl | _blacklist r == M.empty = []
                     | otherwise = [ "blacklist" .= _blacklist r ]

              prob | _probability r == 1 = []
                   | otherwise = [ "probability" .= _probability r ]

-- *Snapshotting

newtype TriggerStateSnapshot = TSS { _ssTriggers :: Map Text Response }

instance FromJSON TriggerStateSnapshot where
  parseJSON = fmap (TSS . mapWithKey doRegex) . parseJSON
    where
      doRegex trig resp | "/" `isPrefixOf` trig && "/" `isSuffixOf` trig && T.length trig >= 2 = doRegex' trig resp
                        | otherwise = resp

      -- Strip the leading and trailing '/', and attempt to compile the inner regex.
      doRegex' trig resp = resp { _regex = either (const Nothing) Just . compile copt eopt . T.unpack . T.init . T.tail $ trig }
      copt = defaultCompOpt { caseSensitive = False }
      eopt = defaultExecOpt

instance ToJSON TriggerStateSnapshot where
  toJSON = toJSON . _ssTriggers

instance Snapshot TriggerState TriggerStateSnapshot where
  snapshotSTM ts = liftM TSS (readTVar $ _triggers ts)

instance Rollback TriggerStateSnapshot TriggerState where
  rollbackSTM tss = liftM TS (newTVar $ _ssTriggers tss)

instance Default TriggerStateSnapshot where
  def = TSS M.empty

-- *Event handler

-- |Attempt to match a trigger against an incoming PRIVMSG and respond.
--
-- Triggers are matched by stripping leading and trailing whitespace
-- and by ignoring case.
eventHandler :: TriggerState -> AsakuraEventHandler
eventHandler ts = AsakuraEventHandler
                    { _description = "Respond to messages consisting of trigger phrases."
                    , _matchType   = EPrivmsg
                    , _eventFunc   = eventFunc ts
                    , _appliesTo   = runEverywhere
                    , _appliesDef  = runAlways
                    }

eventFunc :: TriggerState -> IRCState -> UnicodeEvent -> Bot (IRC ())
eventFunc ts _ ev = return $ do
  let Privmsg _ (Right msg) = _message ev
  network <- _server <$> connectionConfig
  let channel = case _source ev of
                  Channel c _ -> Just c
                  _           -> Nothing

  triggers <- liftIO . atomically . readTVar . _triggers $ ts

  case findTrigger (T.strip msg) triggers of
    Just resp -> respond ev resp network channel
    Nothing   -> return ()

-- Find the first trigger matching this message.
findTrigger :: Text -> Map Text Response -> Maybe Response
findTrigger target = foldWithKey findTrigger' Nothing
  where
    findTrigger' _ _ resp@(Just _) = resp

    findTrigger' trig resp _ =
      case _regex resp of
        Just r  -> if r =~ target                       then Just resp else Nothing
        Nothing -> if T.toLower trig == T.toLower target then Just resp else Nothing

    r =~ txt =
      case execute r (T.unpack txt) of
        Right (Just _) -> True
        _ -> False

respond :: UnicodeEvent -> Response -> ByteString -> Maybe Text -> IRC ()
respond ev r network channel = do
  let blacklisted = case channel of
                      Just chan -> chan `elem` (_blacklist r ^. at (unpack network) . non [])
                      Nothing   -> False

  roll <- liftIO randomIO

  when (not blacklisted && roll <= _probability r) $
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

-- *Updating

-- |Add a new trigger
addTrigger :: MonadIO m => TriggerState -> Text -> Response -> m ()
addTrigger ts trig r = liftIO . atomically $ do
  let tvarT = _triggers ts
  triggers <- readTVar tvarT

  let triggers' = triggers & at (T.toLower trig) .~ Just r
  writeTVar tvarT triggers'

-- |Remove a trigger
removeTrigger :: MonadIO m => TriggerState -> Text -> m ()
removeTrigger ts trig = liftIO . atomically $ do
  let tvarT = _triggers ts
  triggers <- readTVar tvarT

  let triggers' = triggers & at (T.toLower trig) .~ Nothing
  writeTVar tvarT triggers'

-- |Blacklist a trigger in a given channel
blacklist :: MonadIO m => TriggerState -> Text -> ByteString -> Text -> m ()
blacklist ts trig network channel = liftIO . atomically $ do
  let tvarT = _triggers ts
  triggers <- readTVar tvarT

  let triggers' = triggers & at (T.toLower trig) %~ fmap addBL
  writeTVar tvarT triggers'

  where addBL r = r { _blacklist   = _blacklist r & at (unpack network) . non [] %~ (channel:) }

-- |Remove a channel from a trigger's blacklist
whitelist :: MonadIO m => TriggerState -> Text -> ByteString -> Text -> m ()
whitelist ts trig network channel = liftIO . atomically $ do
  let tvarT = _triggers ts
  triggers <- readTVar tvarT

  let triggers' = triggers & at (T.toLower trig) %~ fmap delBL
  writeTVar tvarT triggers'

  where delBL r = r { _blacklist   = _blacklist r & at (unpack network) %~ unblack }
        unblack chans = case filter (/=channel) $ fromMaybe [] chans of
                          [] -> Nothing
                          cs -> Just cs
