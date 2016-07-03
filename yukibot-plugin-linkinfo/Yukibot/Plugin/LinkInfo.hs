{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Yukibot.Plugin.LinkInfo
-- Copyright   : (c) 2016 Michael Walker
-- License     : MIT
-- Stability   : experimental
-- Portability : OverloadedStrings
module Yukibot.Plugin.LinkInfo (linkInfoPlugin) where

import Control.Arrow ((&&&))
import Data.Either (lefts, rights)
import qualified Data.HashMap.Strict as H
import Data.List (nub)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Monoid ((<>))
import Data.Text (Text, unpack, strip)
import qualified Data.Text as T
import Network.URI (URI, parseURI)

import Yukibot.Core

import Yukibot.Plugin.LinkInfo.Common
import qualified Yukibot.Plugin.LinkInfo.HTML as HTML
import qualified Yukibot.Plugin.LinkInfo.Imgur as Imgur
import qualified Yukibot.Plugin.LinkInfo.Youtube as Youtube

linkInfoPlugin :: Table -> Either Text Plugin
linkInfoPlugin cfg =
  let numLinks = fromMaybe 5 $ getInteger "num-links" cfg
  in case (lefts &&& rights) (getHandlers cfg) of
    -- TODO: Gather all errors.
    (err:_, _) -> Left err
    ([], hs)   -> Right (plugin numLinks hs)

-- | The plugin itself. Link handlers are applied in order, and
-- processing stops as soon as one matching handler is found. This
-- allows a priority ordering.
plugin :: Int -> [LinkHandler URI] -> Plugin
plugin numLinks hs = Plugin $ \ev -> case ev of
  Event h (Just c) _ m -> mapM_ (sendAction h . Say c [])  =<< linkTitles m
  Event h Nothing n m  -> mapM_ (sendAction h . Whisper n) =<< linkTitles m

  where
    -- Get a title for every link in a message
    linkTitles :: Text -> IO [Text]
    linkTitles m = do
      let uris = nub $ mapMaybe (parseURI . unpack) (T.words m)
      titles <- mapM fetchLinkInfo uris
      pure . take numLinks $ mapMaybe showTitle titles

    -- Show a title.
    showTitle :: LinkInfo Text -> Maybe Text
    showTitle (Title title) = Just ("Title: \"" <> title <> "\"")
    showTitle (Info  info)  = Just info
    showTitle _ = Nothing

    -- Try to fetch information on a URI.
    fetchLinkInfo :: URI -> IO (LinkInfo Text)
    fetchLinkInfo = fmap unempty . fetch where
      fetch uri = case filter (`lhPredicate` uri) hs of
        (lh:_) -> lhHandler lh uri
        []     -> pure NoTitle
      unempty (Title t) | strip t == "" = NoTitle
      unempty (Info  i) | strip i == "" = NoTitle
      unempty t = t

-- | Get all handlers from the config.
getHandlers :: Table -> [Either Text (LinkHandler URI)]
getHandlers cfg = [get h' (conf h') | h <- getStrings "handlers" cfg, let h' = T.toLower h] where
  -- Instantiate a link handler
  get :: Text -> Table -> Either Text (LinkHandler URI)
  get "html"    = HTML.linkHandler
  get "imgur"   = Imgur.linkHandler
  get "youtube" = Youtube.linkHandler
  get name      = const (Left $ "Unknown link handler: " <> name)

  -- Get the configuration for a link handler.
  conf :: Text -> Table
  conf name = fromMaybe H.empty $ getNestedTable ["handler", name] cfg
