{-# LANGUAGE OverloadedStrings #-}

-- |Provides better titles for imgur links.
--
-- Based on Mathison's imgur linkinfo plugin:
-- <https://github.com/HackSoc/csbot>
module Yukibot.Plugins.LinkInfo.Imgur (imgurLinks) where

import Data.Char (toLower)
import Data.Text (Text)
import Network.URI (URI(..), URIAuth(..))

import Yukibot.Plugins.LinkInfo.Common

import qualified Data.Text as T

-- *LinkInfo integration

imgurLinks :: LinkHandler
imgurLinks = LinkHandler
  { _licName      = "Imgur"
  , _licPredicate = licPredicate
  , _licHandler   = licHandler
  }

-- |LinkInfo predicate: handle any URLs for the domains "imgur.com" or
-- "i.imgur.com".
licPredicate :: URI -> Bool
licPredicate uri = isGalleryUri uri || isImageUri uri

-- |LinkInfo handler: for image URLs, fetch the corresponding gallery
-- page title and, in all cases, return no title if it's the default
-- one.
licHandler :: URI -> IO (LinkInfo Text)
licHandler uri
  | isImageUri   uri = fetchImgurTitle $ toGalleryUri uri
  | isGalleryUri uri = fetchImgurTitle uri
  | otherwise        = return Failed

-- *Utilities

-- |Check if a URI is for a gallery page
isGalleryUri :: URI -> Bool
isGalleryUri uri = case uriAuthority uri of
  Just auth -> uriRegName auth == "imgur.com"
  Nothing   -> False

-- |Check if a URI is for an image
isImageUri :: URI -> Bool
isImageUri uri = case uriAuthority uri of
  Just auth -> uriRegName auth == "i.imgur.com"
  Nothing   -> False

-- |Convert an image URI to a gallery URI
toGalleryUri :: URI -> URI
toGalleryUri uri = uri { uriAuthority = Just newauth, uriPath = newpath } where
  newauth = case uriAuthority uri of
    Just auth -> auth    { uriRegName  = "imgur.com" }
    Nothing   -> URIAuth { uriUserInfo = ""
                        , uriRegName  = "imgur.com"
                        , uriPort     = ""
                        }
  newpath = takeWhile (/='.') $ uriPath uri

-- |Fetch the title of an imgur gallery page, returning Nothing if
-- it's the default title.
fetchImgurTitle :: URI -> IO (LinkInfo Text)
fetchImgurTitle uri = do
  title <- liftHandler fetchTitle uri
  case title of
    Title t
      | "imgur:" `T.isPrefixOf` T.toLower t -> return NoTitle
    other -> return other
