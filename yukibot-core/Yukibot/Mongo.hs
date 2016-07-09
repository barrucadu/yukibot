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
import Database.MongoDB ((=:))
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
--
-- This automatically namespaces by backend signature. The current
-- backend signature is added as a \"_backendsig\" field to the
-- selector.
queryMongo :: MongoConfig -> PluginName -> BackendSig -> M.Selector -> M.Order -> IO [M.Document]
queryMongo cfg pn bsig selBy sortBy =
  let selBy' = addBSig bsig selBy
  in doMongo cfg pn $ \c -> M.rest =<< M.find (M.select selBy' c) { M.sort = sortBy }

-- | Insert values.
--
-- This automatically namespaces by backend signature. The current
-- backend signature is added as a \"_backendsig\" field to the
-- inserted documents.
insertMongo :: MongoConfig -> PluginName -> BackendSig -> [M.Document] -> IO ()
insertMongo cfg pn bsig ds =
  let ds' = map (addBSig bsig) ds
  in doMongo cfg pn $ \c -> M.insertMany_ c ds'

-- | Upsert a value: replace the first document in the selection if
-- there is one; otherwise insert a new document.
--
-- This automatically namespaces by backend signature. The current
-- backend signature is added as a \"_backendsig\" field to the
-- selector and new document.
upsertMongo :: MongoConfig -> PluginName -> BackendSig -> M.Selector -> M.Document -> IO ()
upsertMongo cfg pn bsig selBy doc =
  let selBy' = addBSig bsig selBy
      doc'   = addBSig bsig doc
  in doMongo cfg pn $ \c -> M.upsert (M.Select selBy' c) doc'

-- | Delete values.
--
-- This automatically namespaces by backend signature. The current
-- backend signature is added as a \"_backendsig\" field to the
-- selector.
deleteMongo :: MongoConfig -> PluginName -> BackendSig -> M.Selector -> IO ()
deleteMongo cfg pn bsig selBy =
  let selBy' = addBSig bsig selBy
  in doMongo cfg pn $ \c -> M.delete (M.select selBy' c)

-- | Run a function over a MongoDB database, using the collection
-- belonging to a plugin.
--
-- This does NOT automatically namespace things by the backend
-- signature! Make sure to appropriately manage the \"_backendsig\"
-- field of any documents if you want this scoping!
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

-- | Add the \"_backendsig\" field to a 'Document'. This overwrites
-- any prior \"_backendsig\" field.
addBSig :: BackendSig -> M.Document -> M.Document
addBSig (bname, sname, index) doc = newBSig : removeOldBSig doc where
  removeOldBSig = filter ((/="_backendsig") . M.label)
  newBSig = "_backendsig" =: [ "bname" =: bname
                             , "sname" =: sname
                             , "index" =: index
                             ]
