{-# LANGUAGE OverloadedStrings #-}

-- |Fetch and display titles for HTML pages.
module Yukibot.Plugins.LinkInfo.PageTitle (pageTitle) where

import Control.Monad (liftM)
import Data.Monoid   ((<>))
import Data.Text     (Text)
import Network.URI   (URI(..))
import Yukibot.Plugins.LinkInfo.Common (LinkHandler(..), LinkInfo(..), fetchTitle)

import qualified Data.Text as T

pageTitle :: Int -> LinkHandler
pageTitle maxlen = LinkHandler
                   { _licName      = "pageTitle"
                   , _licPredicate = const True
                   , _licHandler   = licHandler maxlen
                   }

-- |Try to fetch the title of a URL.
--
-- TODO: Don't return a title if it's too similar to the URI
licHandler :: Int -> URI -> IO (LinkInfo Text)
licHandler maxlen = liftM (fmap (trunc maxlen) . maybe NoTitle Title) . fetchTitle

-- |Truncate a Text and append an ellipsis if too long
trunc :: Int -> Text -> Text
trunc maxlen txt | T.length txt > maxlen = T.take (maxlen - 1) txt <> "…"
trunc _ txt = txt
