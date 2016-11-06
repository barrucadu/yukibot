{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Yukibot.Utils
-- Copyright   : (c) 2016 Michael Walker
-- License     : MIT
-- Stability   : experimental
-- Portability : OverloadedStrings
--
-- Extra utilities for writing yukibot plugins.
module Yukibot.Utils where

import Control.Monad.Catch (SomeException, catch)
import Data.Aeson (Object, decode')
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (ByteString, toStrict)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import qualified Network.HTTP.Client.MultipartFormData as WM
import qualified Network.HTTP.Simple as W
import Network.URI (URI)

import Yukibot.Monad
import Yukibot.Plugin.Builtin
import Yukibot.Types

-------------------------------------------------------------------------------
-- * Commands

-- | Make a command only available for deities.
privilegedCommand :: Command -> Command
privilegedCommand cmd = cmd { commandAction = \ev args -> do
  d <- isDeified
  if d
    then commandAction cmd ev args
    else reply =<< notDeityMessage}

-------------------------------------------------------------------------------
-- * HTTP

-- | Download some JSON over HTTP.
downloadJSON :: URI -> IO (Maybe Object)
downloadJSON uri = maybe Nothing decode' <$> download uri

-- | Download a file. Assume UTF-8 encoding.
downloadText :: URI -> IO (Maybe Text)
downloadText uri = maybe Nothing (decodeUtf8 . toStrict) <$> download uri

-- | Download a file.
download :: URI -> IO (Maybe ByteString)
download uri = fetch `catch` handler where
  fetch = do
    req  <- W.parseRequest (show uri)
    resp <- W.httpLbs req
    pure $ if W.getResponseStatusCode resp == 200
      then Just (W.getResponseBody resp)
      else Nothing

  handler :: SomeException -> IO (Maybe a)
  handler = const $ pure Nothing

-- | Upload some text to sprunge and return the response body (the
-- URL).
paste :: String -> IO (Maybe String)
paste txt = upload `catch` handler where
  upload = do
    req  <- W.parseRequest "http://sprunge.us"
    resp <- W.httpLbs =<< WM.formDataBody [WM.partBS "sprunge" (encodeUtf8 (T.pack txt))] req
    pure $ if W.getResponseStatusCode resp == 200
      then filter (/='\n') . T.unpack <$> (decodeUtf8 . toStrict $ W.getResponseBody resp)
      else Nothing

  handler :: SomeException -> IO (Maybe a)
  handler = const $ pure Nothing

-------------------------------------------------------------------------------
-- * Text encoding

-- | Like 'Data.Text.Encoding.decodeUtf8' but doesn't throw an exception.
decodeUtf8 :: BS.ByteString -> Maybe Text
decodeUtf8 = either (const Nothing) Just . decodeUtf8'

-------------------------------------------------------------------------------
-- | Timestamps

-- | Format a timestamp.
showTime :: UTCTime -> Text
showTime = T.pack . formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S"

-- | Format a date.
showDate :: UTCTime -> Text
showDate = T.pack . formatTime defaultTimeLocale "%Y-%m-%d"
