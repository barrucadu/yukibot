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
import Data.Map                   (Map)
import Data.Maybe                 (fromMaybe)
import Data.Text                  (Text)
import Network.IRC.Asakura.Events (runAlways, runEverywhere)
import Network.IRC.Asakura.Types  (AsakuraEventHandler(..), Bot)
import Network.IRC.Asakura.State  (Snapshot(..), Rollback(..))
import Network.IRC.IDTE           (reply)
import Network.IRC.IDTE.Types     ( ConnectionConfig(..)
                                  , Event(..), EventType(EPrivmsg)
                                  , IRC, IRCState
                                  , Message(Privmsg)
                                  , UnicodeEvent
                                  , Source(..)
                                  , connectionConfig)
import System.Random              (randomIO)

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
    }

instance FromJSON Response where
    parseJSON (Object v)= TR <$> v .:  "response"
                             <*> v .:? "blacklist"   .!= M.empty
                             <*> v .:? "probability" .!= 1
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
    parseJSON = fmap TSS . parseJSON

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

  let trigger = T.toLower $ T.strip msg

  case M.lookup trigger triggers of
    Just resp -> respond ev resp network channel
    Nothing   -> return ()

respond :: UnicodeEvent -> Response -> ByteString -> Maybe Text -> IRC ()
respond ev r network channel = do
  let blacklisted = case channel of
                      Just chan -> chan `elem` (_blacklist r ^. at (unpack network) . non [])
                      Nothing   -> False

  roll <- liftIO randomIO

  when (not blacklisted && roll <= _probability r) $
    reply ev $ _response r

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

  where addBL r = TR { _response    = _response r
                     , _blacklist   = _blacklist r & at (unpack network) . non [] %~ (channel:)
                     , _probability = _probability r
                     }

-- |Remove a channel from a trigger's blacklist
whitelist :: MonadIO m => TriggerState -> Text -> ByteString -> Text -> m ()
whitelist ts trig network channel = liftIO . atomically $ do
  let tvarT = _triggers ts
  triggers <- readTVar tvarT

  let triggers' = triggers & at (T.toLower trig) %~ fmap delBL
  writeTVar tvarT triggers'

  where delBL r = TR { _response    = _response r
                     , _blacklist   = _blacklist r & at (unpack network) %~ unblack
                     , _probability = _probability r
                     }
        unblack chans = case filter (/=channel) $ fromMaybe [] chans of
                          [] -> Nothing
                          cs -> Just cs
