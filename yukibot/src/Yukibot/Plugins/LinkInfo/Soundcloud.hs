{-# LANGUAGE OverloadedStrings #-}
module Yukibot.Plugins.LinkInfo.Soundcloud (soundcloudLinks) where

import Control.Applicative ((<$>), (<*>))
import Control.Lens    ((^?), ix, to)
import Data.Aeson      (Object)
import Data.Aeson.Lens (_String, _Integer)
import Data.Char       (toLower)
import Data.List       (isInfixOf)
import Data.Monoid     ((<>))
import Data.Text       (Text, unpack, pack)
import Network.URI     (URI(..), URIAuth(..))
import Yukibot.Plugins.LinkInfo.Common
import Yukibot.Utils

import qualified Data.Text as T

soundcloudLinks :: Maybe Text -> LinkHandler
soundcloudLinks apikey = LinkHandler
  { _licName      = "Soundcloud"
  , _licPredicate = predicate apikey
  , _licHandler   = soundcloud apikey
  }

-- |Check that we can handle a request
predicate :: Maybe Text -> URI -> Bool
predicate Nothing _ = False
predicate _ uri = case uriAuthority uri of
  Just auth -> "soundcloud.com" `isInfixOf` map toLower (uriRegName auth)
  Nothing   -> False

-- |Fetch link info for a soundcloud uri
soundcloud :: Maybe Text -> URI -> IO (LinkInfo Text)
soundcloud Nothing _ = return Failed
soundcloud (Just apikey) uri = getLinkInfo apikey uri

-- |Fetch link info for a soundcloud uri using the given api key
getLinkInfo :: Text -> URI -> IO (LinkInfo Text)
getLinkInfo apikey uri = do
  -- Query the API
  res <- fetchJson apiuri

  -- Extract the information
  let scinfo = res >>= getLinkInfoFromJson

  -- Gracefully handle failure
  return $ maybe Failed Info scinfo

  where
    apiuri = makeUri "api.soundcloud.com" "/resolve.json" . Just $ "?url=" ++ show uri ++ "&client_id=" ++ unpack apikey

-- |Extract the link info from a soundcloud api json response
getLinkInfoFromJson :: Object -> Maybe Text
getLinkInfoFromJson json = formatInfo
  <$> title
  <*> duration
  <*> uploader
  <*> time
  <*> views
  <*> likes

  where
    formatInfo title duration uploader time views likes = T.unwords
      [ "\"" <> title <> "\""
      , "[" <> pack (showSecs $ duration `div` 1000) <> "]"
      , "(by " <> uploader <> " at " <> time <> ")"
      , "|"
      , "Views: " <> pack (show views)
      , "[+" <> pack (show likes) <> "]"
      ]

    title    = json ^? ix "title" . _String
    duration = json ^? ix "duration" . _Integer
    uploader = json ^? ix "user" . ix "username" . _String
    time     = json ^? ix "created_at" . _String . to (T.takeWhile (/=' ') . T.replace "/" "-")
    views    = json ^? ix "playback_count" . _Integer
    likes    = json ^? ix "favoritings_count" . _Integer

    showSecs secs =
      let (m, s) = secs `divMod` 60
      in show m ++ ":" ++ (if s < 10 then "0" else "") ++ show s
