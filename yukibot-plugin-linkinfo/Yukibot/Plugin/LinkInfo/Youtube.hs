{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Yukibot.Plugin.LinkInfo.Youtube
-- Copyright   : (c) 2016 Michael Walker
-- License     : MIT
-- Stability   : experimental
-- Portability : OverloadedStrings
module Yukibot.Plugin.LinkInfo.Youtube where

import Control.Lens ((^?), ix)
import Data.Aeson (Object)
import Data.Aeson.Lens (_String, nth)
import Data.Char (isDigit, toLower)
import Data.List (isInfixOf, stripPrefix, groupBy)
import Data.Function (on)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Monoid ((<>))
import Data.Text (Text, unpack, pack)
import Data.Time.Clock (NominalDiffTime, UTCTime)
import Data.Time.Format (parseTimeM, defaultTimeLocale, formatTime)
import Network.URI (URI(..), URIAuth(..))
import Text.Read (readMaybe)

import Yukibot.Configuration
import Yukibot.Extra

import Yukibot.Plugin.LinkInfo.Common

import qualified Data.Text as T

linkHandler :: Table -> Either Text (LinkHandler URI)
linkHandler cfg = case getString "api-key" cfg of
  Just apiKey -> Right . contramapMaybe getVid $ LinkHandler
    { lhPredicate = const True
    , lhHandler   = getLinkInfo apiKey
    }
  Nothing -> Left "Missing Youtube api-key."

-- | Get the video ID from a URI, if possible.
getVid :: URI -> Maybe Text
getVid uri = case uriAuthority uri of
  Just auth -> extract $ dropWWW auth
  Nothing   -> Nothing

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

-- | Get linkinfo for a video ID
getLinkInfo :: Text -> Text -> IO (LinkInfo Text)
getLinkInfo apikey vid = do
  -- Query the API
  res <- downloadJSON apiuri

  -- Extract the information
  let ytinfo = res >>= getLinkInfoFromJson

  -- Gracefully handle failure
  pure $ maybe Failed Info ytinfo

  where
    apiuri = URI
      { uriScheme    = "https:"
      , uriAuthority = Just URIAuth
        { uriUserInfo = ""
        , uriRegName  = "www.googleapis.com"
        , uriPort     = ""
        }
      , uriPath      = "/youtube/v3/videos"
      , uriQuery     = "?id=" ++ unpack vid ++ "&key=" ++ unpack apikey ++ "&part=snippet,contentDetails,statistics,status"
      , uriFragment  = ""
      }

-- | Extract the link info from a youtube api json response
getLinkInfoFromJson :: Object -> Maybe Text
getLinkInfoFromJson json = formatInfo <$> titleMay
                                      <*> durationMay
                                      <*> uploaderMay
                                      <*> timeMay
                                      <*> viewsMay
                                      <*> likesMay
                                      <*> dislikesMay

  where
    formatInfo title duration uploader time views likes dislikes = T.unwords
      [ "\"" <> title <> "\""
      , "[" <> pack (showDuration duration) <> "]"
      , "(by " <> uploader <> " at " <> pack (formatTime defaultTimeLocale "%F" time) <> ")"
      , "|"
      , "Views: " <> pack (showNum views)
      , "[+" <> pack (showNum likes) <> " -" <> pack (showNum dislikes) <> "]"
      ]

    titleMay    = json ^? ix "items" . nth 0 . ix "snippet"        . ix "title"        . _String
    durationMay = json ^? ix "items" . nth 0 . ix "contentDetails" . ix "duration"     . _String >>= iso8601Duration
    uploaderMay = json ^? ix "items" . nth 0 . ix "snippet"        . ix "channelTitle" . _String
    timeMay     = json ^? ix "items" . nth 0 . ix "snippet"        . ix "publishedAt"  . _String >>= utc
    viewsMay    = json ^? ix "items" . nth 0 . ix "statistics"     . ix "viewCount"    . _String >>= int
    likesMay    = json ^? ix "items" . nth 0 . ix "statistics"     . ix "likeCount"    . _String >>= int
    dislikesMay = json ^? ix "items" . nth 0 . ix "statistics"     . ix "dislikeCount" . _String >>= int

    int = readMaybe . unpack :: Text -> Maybe Int
    utc = parseTimeM True defaultTimeLocale "%FT%X.000%Z" . unpack :: Text -> Maybe UTCTime

-- | Intrepret an ISO 8601 duration string.
iso8601Duration :: Text -> Maybe NominalDiffTime
iso8601Duration = fmap fromIntegral . period . groupBy ((==) `on` isDigit) . unpack where
  period :: [String] -> Maybe Int
  period ("P":ps)  = period ps
  period ("PT":ts) = time ts
  period (ys:"Y":ps)  = (\y r -> r + y * 31536000) <$> readMaybe ys <*> period ps
  period (ys:"YT":ts) = (\y r -> r + y * 31536000) <$> readMaybe ys <*> time   ts
  period (ms:"M":ps)  = (\m r -> r + m * 2628000)  <$> readMaybe ms <*> period ps
  period (ms:"MT":ts) = (\m r -> r + m * 2628000)  <$> readMaybe ms <*> time   ts
  period (ws:"W":ps)  = (\w r -> r + w * 604800)   <$> readMaybe ws <*> period ps
  period (ws:"WT":ts) = (\w r -> r + w * 605800)   <$> readMaybe ws <*> time   ts
  period (ds:"D":ps)  = (\d r -> r + d * 86400)    <$> readMaybe ds <*> period ps
  period (ds:"DT":ts) = (\d r -> r + d * 86400)    <$> readMaybe ds <*> time   ts
  period [] = Just 0
  period _  = Nothing

  time (hs:"H":ts) = (\h r -> r + h * 3600) <$> readMaybe hs <*> time ts
  time (ms:"M":ts) = (\m r -> r + m * 60)   <$> readMaybe ms <*> time ts
  time [ss,"S"]    = readMaybe ss
  time [] = Just 0
  time _  = Nothing

-- | Show a duration nicely
showDuration :: NominalDiffTime -> String
showDuration dur = sign ++ hours ++ mins ++ ":" ++ secs where
  sign = if dur < 0 then "-" else ""

  hrs, ms, ss :: Integer
  (hrs, dur') = abs (round dur) `quotRem` 3600
  (ms,  ss)   = dur' `quotRem` 60

  hours = if hrs /= 0 then show hrs ++ ":" else ""
  mins  = if ms < 10 then "0" ++ show ms else show ms
  secs  = if ss < 10 then "0" ++ show ss else show ss

-- | Show a number, with commas.
showNum :: Show i => i -> String
showNum = reverse . go . reverse . show where
  go xs = case splitAt 3 xs of
    (first, [])   -> first
    (first, rest) -> first ++ "," ++ go rest
