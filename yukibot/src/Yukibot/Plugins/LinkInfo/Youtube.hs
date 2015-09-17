{-# LANGUAGE OverloadedStrings #-}

-- |Better linkinfo for youtube links, stolen again from csbot.
module Yukibot.Plugins.LinkInfo.Youtube (youtubeLinks, getVid) where

import Control.Lens ((^?), ix)
import Data.Aeson (Object)
import Data.Aeson.Lens (_String, nth)
import Data.Char (toLower)
import Data.List (isInfixOf, stripPrefix)
import Data.Maybe (isJust, fromMaybe, listToMaybe, mapMaybe)
import Data.Monoid ((<>))
import Data.Text (Text, unpack, pack)
import Data.Time.Clock (UTCTime)
import Data.Time.Format (parseTimeM, defaultTimeLocale, formatTime)
import Network.URI (URI(..), URIAuth(..))
import Text.Read (readMaybe)

import Yukibot.Plugins.LinkInfo.Common
import Yukibot.Utils

import qualified Data.Text as T

youtubeLinks :: Maybe Text -> LinkHandler
youtubeLinks apikey = LinkHandler
  { _licName      = "Youtube"
  , _licPredicate = isJust . getVid
  , _licHandler   = youtube apikey
  }

-- |Get the video ID from a URI, if possible.
getVid :: URI -> Maybe Text
getVid uri = case uriAuthority uri of
  Just auth -> extract $ dropWWW auth
  Nothing -> Nothing

  where
    path = uriPath uri

    dropWWW auth =
      let dom = map toLower $ uriRegName auth
      in dom `fromMaybe` stripPrefix "www." dom

    extract "youtu.be" = pack <$> stripPrefix "/" path
    extract "youtube.com"
      | "details" `isInfixOf` path = getVParam $ uriQuery uri
      | "watch"   `isInfixOf` path = getVParam $ uriQuery uri
      | otherwise = pack <$> stripPrefix "/v/" path
    extract _ = Nothing

    getVParam = fmap pack . listToMaybe . mapMaybe (stripPrefix "v=") . splitParams
    splitParams = wordsWhen (=='&') . filter (/='?')

-- |Get linkinfo for a url
youtube :: Maybe Text -> URI -> IO (LinkInfo Text)
youtube Nothing _ = return Failed
youtube (Just apikey) uri = maybe (return Failed) (getLinkInfo apikey) $ getVid uri

-- |Get linkinfo for a video ID
getLinkInfo :: Text -> Text -> IO (LinkInfo Text)
getLinkInfo apikey vid = do
  -- Query the API
  res <- fetchJson apiuri

  -- Extract the information
  let ytinfo = res >>= getLinkInfoFromJson

  -- Gracefully handle failure
  return $ maybe Failed Info ytinfo

  where
    apiuri = (makeUri "www.googleapis.com" "/youtube/v3/videos"
               (Just $ "?id=" ++ unpack vid ++ "&key=" ++ unpack apikey ++ "&part=snippet,contentDetails,statistics,status"))
             { uriScheme = "https:" }

-- |Extract the link info from a youtube api json response
getLinkInfoFromJson :: Object -> Maybe Text
getLinkInfoFromJson json = formatInfo
  <$> title
  <*> duration
  <*> uploader
  <*> time
  <*> views
  <*> likes
  <*> dislikes

  where
    formatInfo title duration uploader time views likes dislikes = T.unwords
      [ "\"" <> title <> "\""
      , "[" <> pack (showDuration duration) <> "]"
      , "(by " <> uploader <> " at " <> pack (formatTime defaultTimeLocale "%F" time) <> ")"
      , "|"
      , "Views: " <> pack (showNum views)
      , "[+" <> pack (showNum likes) <> " -" <> pack (showNum dislikes) <> "]"
      ]

    title    = json ^? ix "items" . nth 0 . ix "snippet"        . ix "title"        . _String
    duration = json ^? ix "items" . nth 0 . ix "contentDetails" . ix "duration"     . _String >>= iso8601Duration
    uploader = json ^? ix "items" . nth 0 . ix "snippet"        . ix "channelTitle" . _String
    time     = json ^? ix "items" . nth 0 . ix "snippet"        . ix "publishedAt"  . _String >>= utc
    views    = json ^? ix "items" . nth 0 . ix "statistics"     . ix "viewCount"    . _String >>= int
    likes    = json ^? ix "items" . nth 0 . ix "statistics"     . ix "likeCount"    . _String >>= int
    dislikes = json ^? ix "items" . nth 0 . ix "statistics"     . ix "dislikeCount" . _String >>= int

    int = readMaybe . unpack :: Text -> Maybe Int

    utc = parseTimeM True defaultTimeLocale "%FT%X.000%Z" . unpack :: Text -> Maybe UTCTime
