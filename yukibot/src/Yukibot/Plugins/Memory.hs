{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

-- |Remember facts about nicks.
module Yukibot.Plugins.Memory 
    ( -- *State
      MemoryState
    -- *Snapshotting
    , MemoryStateSnapshot
    -- *Querying
    , getFactValue
    , getFactValues
    , getFacts
    -- *Updating
    , addFactValue
    , setFactValues
    , delFact
    -- *Integration with other things
    , SimpleFactStore(..)
    , simpleFactStore
    , simpleSetCommand
    , simpleGetCommand
    ) where

import Control.Applicative       ((<$>))
import Control.Concurrent.STM    (TVar, atomically, newTVar, readTVar, writeTVar)
import Control.Monad             (liftM)
import Control.Monad.IO.Class    (MonadIO, liftIO)
import Data.Aeson                (FromJSON(..), ToJSON(..))
import Data.Default.Class        (Default(..))
import Data.Map                  (Map)
import Data.Maybe                (fromMaybe, listToMaybe)
import Data.Monoid               ((<>))
import Data.Text                 (Text)
import Network.IRC.Asakura.State (Snapshot(..), Rollback(..))
import Network.IRC.Asakura.Types (Bot)
import Network.IRC.IDTE          (reply)
import Network.IRC.IDTE.Types    (ConnectionConfig(..), Event(..), Source(..), IRCState, IRC, connectionConfig)

import qualified Data.Map  as M
import qualified Data.Text as T

-- *State

-- |Facts about users. A fact belongs to a (network, nick), has a
-- name, and may have a number of values.
type FactStore = Map String (Map Text (Map Text [Text]))

newtype MemoryState = MS { _facts :: TVar FactStore }

-- *Snapshotting

newtype MemoryStateSnapshot = MSS { _msFacts :: FactStore }

instance Default MemoryStateSnapshot where
    def = MSS M.empty

instance ToJSON MemoryStateSnapshot where
    toJSON = toJSON . _msFacts

instance FromJSON MemoryStateSnapshot where
    parseJSON v = MSS <$> parseJSON v

instance Snapshot MemoryState MemoryStateSnapshot where
    snapshotSTM ms = MSS <$> readTVar (_facts ms)

instance Rollback MemoryStateSnapshot MemoryState where
    rollbackSTM mss = MS <$> newTVar (_msFacts mss)

-- *Querying

-- |Get the first value for a fact associated with a user.
getFactValue :: MonadIO m => MemoryState -> String -> Text -> Text -> m (Maybe Text)
getFactValue ms network nick fact = liftM listToMaybe facts
    where facts = getFactValues ms network nick fact

-- |Get all values for a fact associated with a nick.
getFactValues :: MonadIO m => MemoryState -> String -> Text -> Text -> m [Text]
getFactValues ms network nick fact = liftM (concatMap snd . filter relevent) facts
    where facts    = getFacts ms network nick
          relevent = (==fact) . fst

-- |Get all facts associated with a nick. There is no meaningful
-- distinction between a user not having any facts, and a user having
-- an empty list of facts, so this doesn't return a Maybe.
getFacts :: MonadIO m => MemoryState -> String -> Text -> m [(Text, [Text])]
getFacts ms network nick = liftIO . atomically $ do
  facts <- readTVar $ _facts ms
  let userfacts = do
        networkFacts <- M.lookup network facts
        nickFacts    <- M.lookup nick networkFacts
        return $ M.toList nickFacts

  return $ fromMaybe [] userfacts

-- *Updating

-- |Add a fact value to a user. If the fact did not exist, it will be
-- created.
addFactValue :: MonadIO m => MemoryState -> String -> Text -> Text -> Text -> m ()
addFactValue ms network nick fact value = alterFacts ms (Just . (value:)) network nick fact

-- |Replace the fact values associated with a user. If the fact did
-- not exist, it will be created.
setFactValues :: MonadIO m => MemoryState -> String -> Text -> Text -> [Text] -> m ()
setFactValues ms network nick fact values = alterFacts ms (Just . const values) network nick fact

-- |Delete a fact associated with a user. If the fact did not exist,
-- this is a no-op.
delFact :: MonadIO m => MemoryState -> String -> Text -> Text -> m ()
delFact ms = alterFacts ms $ const Nothing

-- |Alter the fact values of a user by a function.
alterFacts :: MonadIO m => MemoryState -> ([Text] -> Maybe [Text]) -> String -> Text -> Text -> m ()
alterFacts ms f network nick fact = liftIO . atomically $ do
  facts <- readTVar $ _facts ms
  writeTVar (_facts ms) $ alterFacts' facts f network nick fact

alterFacts' :: FactStore -> ([Text] -> Maybe [Text]) -> String -> Text -> Text -> FactStore
alterFacts' fs f network nick fact = M.alter (Just . updateNetwork) network fs

    where updateNetwork networkFacts = M.alter (Just . updateNick) nick $ fromMaybe emptyNickMap networkFacts
          updateNick    nickFacts    = M.alter updateFact fact $ fromMaybe emptyFactMap nickFacts

          updateFact factValues   = f $ fromMaybe [] factValues

          emptyNickMap    = M.fromList [(nick, emptyFactMap)]
          emptyFactMap    = M.fromList [(fact, [])]

-- *Integration with other things

-- |A fact store tied to one particular fact.
data SimpleFactStore = SimpleFactStore
    { getSimpleValue :: String -> Text -> IO (Maybe Text)
    -- ^Get the value of the fact
    , setSimpleValue :: String -> Text -> Text -> IO ()
    -- ^Set the value of the fact
    }

-- |Construct a simple fact store.
simpleFactStore :: MemoryState -> Text -> SimpleFactStore
simpleFactStore ms fact = SimpleFactStore
                            { getSimpleValue = \network nick       -> getFactValue  ms network nick fact
                            , setSimpleValue = \network nick value -> setFactValues ms network nick fact [value]
                            }

-- |Construct a command to get the value of a simple fact store for
-- the named user (or the suer themselves, if no nick was given).
--
-- syntax: <prefix><command name> [nick]
simpleGetCommand :: SimpleFactStore -> [Text] -> IRCState -> Event -> Bot (IRC ())
simpleGetCommand sfs args _ ev = return $ do
  let nick = case args of
               (n:_) -> n
               _ -> case _source ev of
                     Channel n _ -> n
                     User n      -> n
                     _           -> "" -- Should never get here

  network <- _server <$> connectionConfig

  value <- liftIO $ getSimpleValue sfs network nick

  reply ev $ fromMaybe ("Couldn't find anything for " <> nick) value

-- |Allow users to set the fact value on themselves.
--
-- Syntax: <prefix><command name> value
simpleSetCommand :: SimpleFactStore -> [Text] -> IRCState -> Event -> Bot (IRC ())
simpleSetCommand sfs args _ ev = return $ do
  let nick = case _source ev of
               Channel n _ -> n
               User n      -> n
               _           -> "" -- Should never get here

  network <- _server <$> connectionConfig

  case args of
    []    -> reply ev "You need to specify a value"
    value -> liftIO $ setSimpleValue sfs network nick $ T.unwords value
