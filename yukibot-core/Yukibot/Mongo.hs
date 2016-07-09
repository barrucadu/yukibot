{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Yukibot.Mongo
-- Copyright   : (c) 2016 Michael Walker
-- License     : MIT
-- Stability   : experimental
-- Portability : OverloadedStrings
module Yukibot.Mongo where

import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Database.MongoDB as M

import Yukibot.Configuration
import Yukibot.Types

-- | Connection information for MongoDB.
data MongoConfig = MongoConfig
  { mongoHost :: M.Host
  -- ^ Hostname (default: \"localhost:27017\")
  , mongoNamespace :: Text
  -- ^ Yukibot collection namespace (default: \"yukibot\")
  }
  deriving (Eq, Ord, Show)

-- | Extract MongoDB configuration.
mongoConfig :: Table -> MongoConfig
mongoConfig cfg =
  let get key def = fromMaybe def $ getString key =<< getTable "mongodb" cfg
      hostname = get "host" "localhost"
      port = maybe M.defaultPort M.PortNumber $ getInteger "port" =<< getTable "mongodb" cfg
  in MongoConfig
     { mongoHost      = M.Host (T.unpack hostname) port
     , mongoNamespace = get "namespace" "yukibot"
     }

-- | Run a query.
queryMongo :: MongoConfig -> PluginName -> M.Selector -> M.Order -> IO [M.Document]
queryMongo cfg pn selBy sortBy = doMongo cfg pn $ \c -> M.rest =<< M.find (M.select selBy c) { M.sort = sortBy }

-- | Insert values.
insertMongo :: MongoConfig -> PluginName -> [M.Document] -> IO ()
insertMongo cfg pn ds = doMongo cfg pn $ \c -> M.insertMany_ c ds

-- | Delete values.
deleteMongo :: MongoConfig -> PluginName -> M.Selector -> IO ()
deleteMongo cfg pn selBy = doMongo cfg pn $ \c -> M.delete (M.select selBy c)

-- | Run a function over a MongoDB database, using the collection
-- belonging to a plugin.
doMongo :: MongoConfig -> PluginName -> (M.Collection -> M.Action IO a) -> IO a
doMongo cfg pn cf = do
  pipe <- M.connect $ mongoHost cfg
  let c = collectionFor cfg pn
  res <- M.access pipe M.master c (cf c)
  M.close pipe
  pure res

-- | Get the collection name for the given plugin.
collectionFor :: MongoConfig -> PluginName -> M.Collection
collectionFor cfg pn
  | T.null (mongoNamespace cfg) = getPluginName pn
  | otherwise = mongoNamespace cfg <> "_" <> getPluginName pn
