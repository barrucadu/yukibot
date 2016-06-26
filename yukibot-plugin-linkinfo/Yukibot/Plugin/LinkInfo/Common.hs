{-# LANGUAGE DeriveFunctor #-}

-- |
-- Module      : Yukibot.Plugin.LinkInfo.Common
-- Copyright   : (c) 2016 Michael Walker
-- License     : MIT
-- Stability   : experimental
-- Portability : DeriveFunctor
module Yukibot.Plugin.LinkInfo.Common where

import Control.Monad.Catch (SomeException, catch)
import Data.ByteString.Lazy (toStrict)
import Data.Text (Text, pack, strip, unpack)
import Data.Text.Encoding (decodeUtf8)
import Text.XML.HXT.Core ((//>), readString, hasName, getText, runX, withParseHTML, withWarnings, yes, no)
import Text.XML.HXT.TagSoup (withTagSoup)
import qualified Network.HTTP.Simple as W
import Network.URI (URI)

data LinkHandler = LinkHandler
  { lhPredicate :: Text -> Bool
  -- ^ When to apply this handler
  , lhHandler :: Text -> IO (LinkInfo Text)
  -- ^ Get link info from a URI
  }

data LinkInfo a
  = Title a -- ^ Title to display, in quotes.
  | Info a  -- ^ Information to display verbatim.
  | NoTitle -- ^ The URI has no title.
  | Failed  -- ^ Failed to retrieve the title
  deriving (Eq, Ord, Read, Show, Functor)

-- | Fetch the title of a URI.
fetchTitle :: Text -> IO (Maybe Text)
fetchTitle uri = do
  downloaded <- download uri
  case downloaded of
    Just html -> do
      let doc = readString [ withParseHTML yes
                           , withTagSoup
                           , withWarnings  no
                           ] (unpack html)
      title <- runX $ doc //> hasName "title" //> getText
      pure . Just . strip . pack . toPlainText . concat $ title
    Nothing -> pure Nothing

  where
    toPlainText = dequote . unwords . words

    dequote ('\"':xs) | last xs == '\"' = (init . tail) xs
    dequote xs = xs

fetchTitleUri :: URI -> IO (Maybe Text)
fetchTitleUri = fetchTitle . pack . show

-- | Download a file. Assume UTF-8 encoding.
download :: Text -> IO (Maybe Text)
download uri = fetch `catch` handler where
  fetch = do
    req  <- W.parseRequest (unpack uri)
    resp <- W.httpLbs req
    pure $ if W.getResponseStatusCode resp == 200
      then Just . decodeUtf8 . toStrict $ W.getResponseBody resp
      else Nothing

  handler :: SomeException -> IO (Maybe a)
  handler = const $ pure Nothing
