{-# LANGUAGE OverloadedStrings     #-}

-- |Remember facts about nicks.
module Yukibot.Plugins.Memory
    ( -- *Querying
      getFactValue
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
import Database.MongoDB          (Document, (=:), at, insertMany_)
import Network.IRC.Asakura.Commands (CommandDef(..))
import Network.IRC.Client        (reply)
import Network.IRC.Client.Types  (ConnectionConfig(..), Event(..), Source(..), connectionConfig)
import Yukibot.Utils

import qualified Data.Text as T

-- *Querying

-- |Get the first value for a fact associated with a user.
getFactValue :: MonadIO m => Mongo -> ByteString -> Text -> Text -> m (Maybe Text)
getFactValue ms network nick fact = liftM listToMaybe facts
    where facts = getFactValues ms network nick fact

-- |Get all values for a fact associated with a nick.
getFactValues :: MonadIO m => Mongo -> ByteString -> Text -> Text -> m [Text]
getFactValues mongo network nick fact = map (at "value") `liftM` queryMongo mongo selector ordering
  where
    selector = ["network" =: unpack network, "nick" =: nick, "fact" =: fact]
    ordering = ["timestamp" =: (-1 :: Int)]

-- |Get all facts associated with a nick. There is no meaningful
-- distinction between a user not having any facts, and a user having
-- an empty list of facts, so this doesn't return a Maybe.
getFacts :: MonadIO m => Mongo -> ByteString -> Text -> m [(Text, [Text])]
getFacts mongo network nick = toFactList `liftM` queryMongo mongo selector ordering
  where
    selector = ["network" =: unpack network, "nick" =: nick]
    ordering = ["fact" =: (1 :: Int), "timestamp" =: (-1 :: Int)]
    toFactList = map (\fs@(f:_) -> (at "fact" f, map (at "value") fs)) . groupBy ((==) `on` (at "fact" :: Document -> Text))

-- *Updating

-- |Add a fact value to a user. If the fact did not exist, it will be
-- created.
addFactValue :: MonadIO m => Mongo -> ByteString -> Text -> Text -> Text -> m ()
addFactValue ms network nick fact value = alterFacts ms (Just . (value:)) network nick fact

-- |Replace the fact values associated with a user. If the fact did
-- not exist, it will be created.
setFactValues :: MonadIO m => Mongo -> ByteString -> Text -> Text -> [Text] -> m ()
setFactValues ms network nick fact values = alterFacts ms (Just . const values) network nick fact

-- |Delete a fact associated with a user. If the fact did not exist,
-- this is a no-op.
delFact :: MonadIO m => Mongo -> ByteString -> Text -> Text -> m ()
delFact ms = alterFacts ms $ const Nothing

-- |Alter the fact values of a user by a function.
alterFacts :: MonadIO m => Mongo -> ([Text] -> Maybe [Text]) -> ByteString -> Text -> Text -> m ()
alterFacts mongo f network nick fact = liftIO $ do
  now <- getCurrentTime
  doMongo mongo $ alter now

  where
    alter now c = do
      facts <- queryMongo mongo ["network" =: unpack network, "nick" =: nick, "fact" =: fact] []
      let newvals = f $ map (at "value") facts
      insertMany_ c
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
simpleFactStore :: Mongo -> Text -> SimpleFactStore
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
