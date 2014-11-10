{-# LANGUAGE OverloadedStrings #-}

-- |Better linkinfo for youtube links, stolen again from csbot.
module Yukibot.Plugins.LinkInfo.Youtube (youtubeLinks, getVid) where

import Control.Applicative ((<$>), (<*>))
import Control.Lens    ((^?), ix, to)
import Data.Aeson      (Object)
import Data.Aeson.Lens (_String, nth)
import Data.Char       (toLower)
import Data.List       (isInfixOf, stripPrefix)
import Data.Maybe      (isJust, fromMaybe, listToMaybe, mapMaybe)
import Data.Monoid     ((<>))
import Data.Text       (Text, unpack, pack)
import Network.URI     (URI(..), URIAuth(..))
import Text.Read       (readMaybe)
import Yukibot.Plugins.LinkInfo.Common
import Yukibot.Utils

import qualified Data.Text as T

youtubeLinks :: LinkHandler
youtubeLinks = LinkHandler
  { _licName      = "Youtube"
  , _licPredicate = isJust . getVid
  , _licHandler   = youtube
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
youtube :: URI -> IO (LinkInfo Text)
youtube uri = maybe (return Failed) getLinkInfo $ getVid uri

-- |Get linkinfo for a video ID
getLinkInfo :: Text -> IO (LinkInfo Text)
getLinkInfo vid = do
  -- Query the API
  res <- fetchJson apiuri

  -- Extract the information
  let ytinfo = res >>= getLinkInfoFromJson

  -- Gracefully handle failure
  return $ maybe Failed Info ytinfo

  where
    apiuri = (makeUri "gdata.youtube.com" ("/feeds/api/videos/" ++ unpack vid) (Just "?alt=json&v=2"))
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
      , "[" <> pack (showSecs duration) <> "]"
      , "(by " <> uploader <> " at " <> time <> ")"
      , "|"
      , "Views: " <> pack (show views)
      , "[+" <> pack (show likes) <> " -" <> pack (show dislikes) <> "]"
      ]

    title    = json ^? ix "entry" . ix "media$group" . ix "media$title" . ix "$t" . _String
    duration = json ^? ix "entry" . ix "media$group" . ix "yt$duration" . ix "seconds" . _String >>= int
    uploader = json ^? ix "entry" . ix "author" . nth 0 . ix "name" . ix "$t" . _String
    time     = json ^? ix "entry" . ix "media$group" . ix "yt$uploaded" . ix "$t" . _String . to (T.takeWhile (/='T'))
    views    = json ^? ix "entry" . ix "yt$statistics" . ix "viewCount" . _String >>= int
    likes    = json ^? ix "entry" . ix "yt$rating" . ix "numLikes" . _String >>= int
    dislikes = json ^? ix "entry" . ix "yt$rating" . ix "numDislikes" . _String >>= int

    int = readMaybe . unpack :: Text -> Maybe Int

    showSecs secs =
      let (m, s) = secs `divMod` 60
      in show m ++ ":" ++ (if s < 10 then "0" else "") ++ show s
