{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Yukibot.Plugin.LinkInfo.Imgur
-- Copyright   : (c) 2016 Michael Walker
-- License     : MIT
-- Stability   : experimental
-- Portability : OverloadedStrings
module Yukibot.Plugin.LinkInfo.Imgur where

import Data.Text (Text, unpack)
import qualified Data.Text as T
import Network.URI (URI(..), URIAuth(..), parseURI)

import Yukibot.Plugin.LinkInfo.Common

linkHandler :: config -> Either error LinkHandler
linkHandler _ = Right LinkHandler
  { lhPredicate = predicate
  , lhHandler   = handler
  }

-- | Handle any URLs for the domains "imgur.com" or "i.imgur.com".
predicate :: Text -> Bool
predicate turi = case parseURI (unpack turi) of
  Just uri -> isImageUri uri || isGalleryUri uri
  Nothing -> False

-- | fetch an image title.
handler :: Text -> IO (LinkInfo Text)
handler turi = case parseURI (unpack turi) of
  Just uri | isImageUri   uri -> fetchImgurTitle (toGalleryUri uri)
           | isGalleryUri uri -> fetchImgurTitle uri
  _ -> pure Failed

-- | Check if a URI is for "i.imgur.com" (an image).
isImageUri :: URI -> Bool
isImageUri uri = case uriAuthority uri of
  Just auth -> uriRegName auth == "i.imgur.com"
  Nothing -> False

-- | Check if a URI is for "imgur.com" (the gallery).
isGalleryUri :: URI -> Bool
isGalleryUri uri = case uriAuthority uri of
  Just auth -> uriRegName auth == "imgur.com"
  Nothing -> False

-- | Convert an image URI into a gallery URI.
toGalleryUri :: URI -> URI
toGalleryUri uri = uri { uriAuthority = Just newAuth, uriPath = newPath } where
  newAuth = case uriAuthority uri of
    Just auth -> auth { uriRegName = "imgur.com" }
    Nothing   -> URIAuth { uriUserInfo = ""
                         , uriRegName  = "imgur.com"
                         , uriPort     = ""
                         }

  newPath = takeWhile (/='.') (uriPath uri)

-- | Fetch the title of an imgur gallery page, returning @NoTitle@ if
-- it's the default.
fetchImgurTitle :: URI -> IO (LinkInfo Text)
fetchImgurTitle uri = do
  title <- fetchTitleUri uri
  pure $ case title of
    Just t | "imgur:" `T.isPrefixOf` T.toLower t -> NoTitle
           | otherwise -> Title t
    Nothing -> Failed
