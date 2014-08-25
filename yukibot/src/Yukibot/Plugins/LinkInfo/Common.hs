{-# LANGUAGE OverloadedStrings #-}

-- |Common functions for LinkInfo plugins
module Yukibot.Plugins.LinkInfo.Common where

import Control.Applicative        ((<$>), (<*>), pure)
import Control.Monad.IO.Class     (MonadIO, liftIO)
import Data.Aeson                 (FromJSON(..), ToJSON(..), Value(..), (.=), (.:?), (.!=), object)
import Data.Default.Class         (Default(..))
import Data.Maybe                 (listToMaybe)
import Data.Text                  (Text, pack, unpack)
import Text.XML.HXT.Core          ((//>), readString, hasName, getText, runX, withParseHTML, withWarnings, no, yes)
import Text.XML.HXT.TagSoup       (withTagSoup)
import Yukibot.Utils              (fetchHtml)

-- *Configuration

-- |Currently configurable settings: number of link titles to show,
-- and length limit of titles.
data LinkInfoCfg = LIC
    { _numLinks     :: Int
    , _maxTitleLen  :: Int
    , _linkHandlers :: [(Text -> Bool, Text -> IO (Maybe Text))]
    -- ^Link handlers are used for providing site-specific
    -- information.
    }

instance ToJSON LinkInfoCfg where
    toJSON cfg = object [ "numLinks" .= _numLinks    cfg
                        , "maxLen"   .= _maxTitleLen cfg
                        ]

instance FromJSON LinkInfoCfg where
    parseJSON (Object v) = LIC <$> v .:? "numLinks" .!= _numLinks    def
                               <*> v .:? "maxLen"   .!= _maxTitleLen def
                               <*> pure []
    parseJSON _ = fail "Expected object"

instance Default LinkInfoCfg where
    def = LIC { _numLinks     = 5
              , _maxTitleLen  = 100
              , _linkHandlers = []
              }

-- *Link handlers

-- |Add a new link handler
addLinkHandler :: LinkInfoCfg
               -> (Text -> Bool)
               -- ^Predicate function, applied to the URI.
               -> (Text -> IO (Maybe Text))
               -- ^Link info function, applied if the predicate matches.
               -> LinkInfoCfg
addLinkHandler lic p h = LIC { _numLinks     = _numLinks lic
                             , _maxTitleLen  = _maxTitleLen lic
                             , _linkHandlers = (p, h) : _linkHandlers lic
                             }

-- |Find the handler for a URI.
getLinkHandler :: LinkInfoCfg -> Text -> Maybe (Text -> IO (Maybe Text))
getLinkHandler lic uri = listToMaybe . map snd . filter (($uri) . fst) . _linkHandlers $ lic

-- *Title fetching

-- |Try to fetch the title of a URL.
fetchTitle :: MonadIO m => Text -> m (Maybe Text)
fetchTitle url = liftIO $ do
  downloaded <- fetchHtml $ unpack url
  case downloaded of
    Just html -> do
      let doc = readString [ withParseHTML yes
                           , withTagSoup
                           , withWarnings  no
                           ] html

      title <- runX $ doc //> hasName "title" //> getText
      return . Just . pack . toPlainText . concat $ title

    Nothing -> return Nothing

  where toPlainText = dequote . unwords . words
        dequote ('\"':xs) | last xs == '\"' = init xs
        dequote x = x

