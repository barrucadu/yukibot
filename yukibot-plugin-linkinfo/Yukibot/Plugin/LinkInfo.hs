{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Yukibot.Plugin.LinkInfo
-- Copyright   : (c) 2016 Michael Walker
-- License     : MIT
-- Stability   : experimental
-- Portability : OverloadedStrings
--
-- A link information plugin for yukibot-core. This provides one
-- monitor and one command:
--
--     * Monitor "linkinfo", responds with the title of every link in
--       a message (up to some limit)
--
--     * Command "linkinfo", which responds with the title of every
--       link in a message.
module Yukibot.Plugin.LinkInfo (linkInfoPlugin) where

import Control.Arrow ((&&&))
import Control.Monad.IO.Class (MonadIO, liftIO)
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
plugin numLinks hs = Plugin
  { pluginHelp = "fetch link titles"
  , pluginMonitors = H.fromList [("linkinfo", Monitor "fetch titles for URLs in messages" $ linkinfo True)]
  , pluginCommands = H.fromList [("linkinfo", Command "fetch ttiles for the given URLs" $ \ev _ -> linkinfo False ev)]
  }

  where
    -- Respond with linkinfo for a collection of links.
    linkinfo lim ev = mapM_ reply  =<< linkTitles lim (eventMessage ev)

    -- Get a title for every link in a message
    linkTitles :: MonadIO m => Bool -> Text -> m [Text]
    linkTitles lim m = liftIO $ do
      let uris = nub $ mapMaybe (parseURI . unpack) (T.words m)
      titles <- mapMaybe showTitle <$> mapM fetchLinkInfo uris
      pure $ if lim then take numLinks titles else titles

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
