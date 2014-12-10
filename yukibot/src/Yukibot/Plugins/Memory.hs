{-# LANGUAGE OverloadedStrings     #-}

-- |Remember facts about nicks.
module Yukibot.Plugins.Memory
    ( MemoryState(..)
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
import Control.Monad             (liftM)
import Control.Monad.IO.Class    (MonadIO, liftIO)
import Data.ByteString           (ByteString)
import Data.ByteString.Char8     (unpack)
import Data.List                 (groupBy)
import Data.Function             (on)
import Data.Maybe                (fromMaybe, listToMaybe)
import Data.Monoid               ((<>))
import Data.Text                 (Text)
import Data.Time.Clock           (getCurrentTime)
import Database.MongoDB          ((=:), Collection, Document, access, at, close, connect, host, master, find, rest, select, sort, insertMany)
import Network.IRC.Asakura.Commands (CommandDef(..))
import Network.IRC.Client        (reply)
import Network.IRC.Client.Types  (ConnectionConfig(..), Event(..), Source(..), connectionConfig)

import qualified Data.Text as T

-- *Wrapper for the Mongo connection info, storing a hostname and collection name
newtype MemoryState = MS (String, Collection)

-- *Querying

-- |Get the first value for a fact associated with a user.
getFactValue :: MonadIO m => MemoryState -> ByteString -> Text -> Text -> m (Maybe Text)
getFactValue ms network nick fact = liftM listToMaybe facts
    where facts = getFactValues ms network nick fact

-- |Get all values for a fact associated with a nick.
getFactValues :: MonadIO m => MemoryState -> ByteString -> Text -> Text -> m [Text]
getFactValues (MS (h, c)) network nick fact = liftIO $ do
  pipe <- connect $ host h
  res <- access pipe master c mongo
  close pipe
  return $ map (at "value") res

  where
    mongo = rest =<< find (select query c) {sort = ["timestamp" =: (-1 :: Int)]}

    query =
      [ "network" =: unpack network
      , "nick"    =: nick
      , "fact"    =: fact
      ]

-- |Get all facts associated with a nick. There is no meaningful
-- distinction between a user not having any facts, and a user having
-- an empty list of facts, so this doesn't return a Maybe.
getFacts :: MonadIO m => MemoryState -> ByteString -> Text -> m [(Text, [Text])]
getFacts (MS (h, c)) network nick = liftIO $ do
  pipe <- connect $ host h
  res <- access pipe master c mongo
  close pipe
  return $ toFactList res

  where
    mongo = rest =<< find (select query c) {sort = ["fact" =: (1 :: Int), "timestamp" =: (-1 :: Int)]}

    query =
      [ "network" =: unpack network
      , "nick"    =: nick
      ]

    toFactList = map (\fs@(f:_) -> (at "fact" f, map (at "value") fs)) . groupBy ((==) `on` (at "fact" :: Document -> Text))

-- *Updating

-- |Add a fact value to a user. If the fact did not exist, it will be
-- created.
addFactValue :: MonadIO m => MemoryState -> ByteString -> Text -> Text -> Text -> m ()
addFactValue ms network nick fact value = alterFacts ms (Just . (value:)) network nick fact

-- |Replace the fact values associated with a user. If the fact did
-- not exist, it will be created.
setFactValues :: MonadIO m => MemoryState -> ByteString -> Text -> Text -> [Text] -> m ()
setFactValues ms network nick fact values = alterFacts ms (Just . const values) network nick fact

-- |Delete a fact associated with a user. If the fact did not exist,
-- this is a no-op.
delFact :: MonadIO m => MemoryState -> ByteString -> Text -> Text -> m ()
delFact ms = alterFacts ms $ const Nothing

-- |Alter the fact values of a user by a function.
alterFacts :: MonadIO m => MemoryState -> ([Text] -> Maybe [Text]) -> ByteString -> Text -> Text -> m ()
alterFacts (MS (h, c)) f network nick fact = liftIO $ do
  now <- getCurrentTime
  pipe <- connect $ host h
  access pipe master c $ mongo now
  close pipe

  where
    mongo now = do
      facts <- rest =<< find (select ["network" =: unpack network, "nick" =: nick, "fact" =: fact] c)
      let newvals = f $ map (at "value") facts
      insertMany c
        [ ["network" =: unpack network, "nick" =: nick, "fact" =: fact, "value" =: val, "timestamp" =: now]
        | val <- [] `fromMaybe` newvals
        ]

-- *Integration with other things

-- |A fact store tied to one particular fact.
data SimpleFactStore = SimpleFactStore
    { getSimpleValue :: ByteString -> Text -> IO (Maybe Text)
    -- ^Get the value of the fact
    , setSimpleValue :: ByteString -> Text -> Text -> IO ()
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
simpleGetCommand :: SimpleFactStore -> CommandDef
simpleGetCommand sfs = CommandDef { _verb   = ["get"]
                                  , _help   = "<nick> - Look up the value associated with the nick."
                                  , _action = go
                                  }

  where
    go args _ ev = return $ do
      let nick = case args of
                   (n:_) -> n
                   _ -> case _source ev of
                         Channel _ n -> n
                         User n      -> n
                         _           -> "" -- Should never get here

      network <- _server <$> connectionConfig

      value <- liftIO $ getSimpleValue sfs network nick

      reply ev $ fromMaybe ("Couldn't find anything for " <> nick) value

-- |Allow users to set the fact value on themselves.
--
-- Syntax: <prefix><command name> value
simpleSetCommand :: SimpleFactStore -> CommandDef
simpleSetCommand sfs = CommandDef { _verb   = ["set"]
                                  , _help   = "<value> - Update the value associated with your nick."
                                  , _action = go
                                  }

  where
    go args _ ev = return $ do
      let nick = case _source ev of
                   Channel _ n -> n
                   User n      -> n
                   _           -> "" -- Should never get here

      network <- _server <$> connectionConfig

      case args of
        []    -> reply ev "You need to specify a value"
        value -> liftIO $ setSimpleValue sfs network nick $ T.unwords value
