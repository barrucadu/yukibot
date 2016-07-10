{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Yukibot.Plugin.LinkInfo.Imgur
-- Copyright   : (c) 2016 Michael Walker
-- License     : MIT
-- Stability   : experimental
-- Portability : OverloadedStrings
module Yukibot.Plugin.LinkInfo.Imgur where

import Data.Text (Text)
import qualified Data.Text as T
import Network.URI (URI(..), URIAuth(..))

import Yukibot.Plugin.LinkInfo.Common

linkHandler :: config -> Either error (LinkHandler URI)
linkHandler _ = Right . contramapMaybe getGalleryUri $ LinkHandler
  { lhPredicate = const True
  , lhHandler   = getLinkInfo
  }

-- | Get the gallery URI for an imgur link.
getGalleryUri :: URI -> Maybe URI
getGalleryUri uri
  | isImageUri   = Just uri { uriAuthority = Just galleryAuth, uriPath = galleryPath }
  | isGalleryUri = Just uri
  | otherwise = Nothing

  where
    -- Check if a URI is for "i.imgur.com" (an image).
    isImageUri = maybe False ((=="i.imgur.com") . uriRegName) (uriAuthority uri)

    -- Check if a URI is for "imgur.com" (the gallery).
    isGalleryUri = maybe False ((=="imgur.com") . uriRegName) (uriAuthority uri)

    -- The URI authority of the imgur gallery.
    galleryAuth = case uriAuthority uri of
      Just auth -> auth { uriRegName = "imgur.com" }
      Nothing   -> URIAuth { uriUserInfo = ""
                           , uriRegName  = "imgur.com"
                           , uriPort     = ""
                           }

    -- The URI path of the image in the gallery
    galleryPath = takeWhile (/='.') (uriPath uri)

-- | Fetch the title of an imgur gallery page, returning @NoTitle@ if
-- it's the default.
getLinkInfo :: URI -> IO (LinkInfo Text)
getLinkInfo uri = do
  title <- fetchTitle uri
  pure $ case title of
    Just t | "imgur:" `T.isPrefixOf` T.toLower t -> NoTitle
           | otherwise -> Title t
    Nothing -> Failed
