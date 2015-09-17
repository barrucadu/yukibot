{-# LANGUAGE OverloadedStrings #-}

-- |Fetch and display titles for HTML pages.
module Yukibot.Plugins.LinkInfo.PageTitle (pageTitle) where

import Data.Char (toLower)
import Data.List (isInfixOf, isPrefixOf)
import Data.Monoid ((<>))
import Data.Text (Text, unpack)
import Network.URI (URI(..), URIAuth(..))

import Yukibot.Plugins.LinkInfo.Common
import Yukibot.Utils

import qualified Data.Text as T

pageTitle :: Int -> LinkHandler
pageTitle maxlen = LinkHandler
  { _licName      = "pageTitle"
  , _licPredicate = filterTypes
  , _licHandler   = licHandler maxlen
  }

-- |Don't fetch titles for URLs which look like a media file.
filterTypes :: URI -> Bool
filterTypes uri = not $ any (`isPrefixOf` uriPath uri) types where
  types = [".png",".jpg",".jpeg",".gif",".mp3",".mp4",".wav",".avi",".mkv"]

-- |Try to fetch the title of a URL.
licHandler :: Int -> URI -> IO (LinkInfo Text)
licHandler maxlen uri = do
  title <- fmap (trunc maxlen) . maybe NoTitle Title <$> fetchTitle uri
  return $ case title of
    Title t | not $ isSimilar t uri -> title
    _ -> NoTitle

-- |Truncate a Text and append an ellipsis if too long
trunc :: Int -> Text -> Text
trunc maxlen txt | T.length txt > maxlen = T.take (maxlen - 1) txt <> "…"
trunc _ txt = txt

-- |Check that a title and uri aren't too similar
isSimilar :: Text -> URI -> Bool
isSimilar title uri = inDomain || inPath || inTitle where
  -- Normalise case and strip non-[a-z/]s
  slug      = normalise $ unpack title
  path      = normalise $ uriPath uri
  domain    = maybe "" (normalise . uriRegName) $ uriAuthority uri
  normalise = filter (`notElem` ('/':['a'..'z'])) . map toLower

  -- Is the slug in the domain name?
  inDomain = slug `isInfixOf` domain

  -- Is the slug in the path?
  inPath = slug `isInfixOf` path

  -- Is some part of the path the start of the title?
  inTitle = any (\c -> c `isPrefixOf` slug && sluglen <= length c * 2) chunks
  chunks  = filter ((<10) . length) $ wordsWhen (=='/') path
  sluglen = length slug
