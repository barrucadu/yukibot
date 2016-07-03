{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Yukibot.Plugin.LinkInfo.HTML
-- Copyright   : (c) 2016 Michael Walker
-- License     : MIT
-- Stability   : experimental
-- Portability : OverloadedStrings
module Yukibot.Plugin.LinkInfo.HTML where

import Data.Char (toLower)
import Data.List (isInfixOf, isPrefixOf, isSuffixOf)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text, unpack)
import qualified Data.Text as T
import Network.URI (URI(..), URIAuth(..))

import Yukibot.Configuration

import Yukibot.Plugin.LinkInfo.Common

linkHandler :: Table -> Either error LinkHandler
linkHandler cfg =
  let maxTitleLen = fromMaybe 100 $ getInteger "max-title-len" cfg
  in Right LinkHandler
     { lhPredicate = predicate
     , lhHandler   = handler maxTitleLen
     }

-- | Don't fetch titles for URIs which look like a media file.
predicate :: URI -> Bool
predicate uri = not $ any (`isSuffixOf` uriPath uri) types where
  types = [".bmp", ".png", ".jpg", ".jpeg", ".gif", ".mp3", ".mp4", ".wav", ".avi", ".mkv"]

-- | Try to fetch the title of a URL. Drop titles which are too
-- similar to the URI.
handler :: Int -> URI -> IO (LinkInfo Text)
handler maxTitleLen uri = do
  title <- fmap (trunc maxTitleLen) . maybe Failed Title <$> fetchTitle uri
  pure $ case title of
    Title t | not (isSimilar t uri) -> title
    _ -> NoTitle

-- | Truncate a Text and append an ellipsis if too long.
trunc :: Int -> Text -> Text
trunc maxLen txt | T.length txt > maxLen = T.take (maxLen - 1) txt <> "…"
trunc _ txt = txt

-- | Check that a title and URL aren't too similar.
isSimilar :: Text -> URI -> Bool
isSimilar title uri = inDomain || inPath || inTitle where
  -- Normalise case and strip non-[a-z/]s
  slug      = normalise $ unpack title
  path      = normalise $ uriPath uri
  domain    = maybe "" (normalise . uriRegName) $ uriAuthority uri
  normalise = filter (`elem` ('/':['a'..'z'])) . map toLower

  -- Is the slug in the domain name?
  inDomain = slug `isInfixOf` domain

  -- Is the slug in the path?
  inPath = slug `isInfixOf` path

  -- Is some part of the path the start of the title?
  inTitle = any (\c -> isPrefixOf c slug && length slug <= length c * 2) chunks
  chunks = filter ((<10) . length) $ wordsWhen (=='/') path

-- | Split a string by a character.
wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
  "" -> []
  s' -> let (w, s'') = break p s'
        in w : wordsWhen p s''
