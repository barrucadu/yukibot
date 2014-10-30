{-# LANGUAGE OverloadedStrings #-}

-- |Fetch and display titles for HTML pages.
module Yukibot.Plugins.LinkInfo.PageTitle (pageTitle) where

import Control.Monad (liftM)
import Data.Monoid   ((<>))
import Data.Text     (Text)
import Network.URI   (URI(..))
import Yukibot.Plugins.LinkInfo.Common (LinkHandler(..), LinkInfo(..), LinkInfoCfg(..), fetchTitle)

import qualified Data.Text as T

pageTitle :: LinkInfoCfg -> LinkHandler
pageTitle lic = LinkHandler
                { _licPredicate = const True
                , _licHandler   = licHandler lic
                }

-- |Try to fetch the title of a URL.
--
-- TODO: Don't return a title if it's too similar to the URI
licHandler :: LinkInfoCfg -> URI -> IO (LinkInfo Text)
licHandler lic = liftM (fmap (trunc $ _maxTitleLen lic) . maybe NoTitle Title) . fetchTitle

-- |Truncate a Text and append an ellipsis if too long
trunc :: Int -> Text -> Text
trunc maxlen txt | T.length txt > maxlen = T.take (maxlen - 1) txt <> "…"
trunc _ txt = txt
