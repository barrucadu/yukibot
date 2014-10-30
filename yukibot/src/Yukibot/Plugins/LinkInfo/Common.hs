{-# LANGUAGE OverloadedStrings #-}

-- |Common functions for LinkInfo plugins
module Yukibot.Plugins.LinkInfo.Common where

import Control.Applicative    ((<$>), (<*>), pure)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson             (FromJSON(..), ToJSON(..), Value(..), (.=), (.:?), (.!=), object)
import Data.Default.Class     (Default(..))
import Data.Maybe             (listToMaybe)
import Data.Text              (Text, pack, strip)
import Text.XML.HXT.Core      ((//>), readString, hasName, getText, runX, withParseHTML, withWarnings, no, yes)
import Text.XML.HXT.TagSoup   (withTagSoup)
import Network.URI            (URI)
import Yukibot.Utils          (fetchHtml)

-- *Configuration

-- |Currently configurable settings: number of link titles to show,
-- and length limit of titles.
data LinkInfoCfg = LIC
    { _numLinks     :: Int
    , _maxTitleLen  :: Int
    , _linkHandlers :: [(URI -> Bool, URI -> IO (LinkInfo Text))]
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

data LinkInfo a = Title a -- ^This is the title to be displayed.
                | Info a  -- ^Information to display, and don't also
                          -- display the uri.
                | NoTitle -- ^The URI has no title.
                | Failed  -- ^Retrieving the title failed.

instance Functor LinkInfo where
    fmap f (Title a) = Title $ f a
    fmap f (Info a)  = Info $ f a
    fmap _ NoTitle   = NoTitle
    fmap _ Failed    = Failed

-- |Add a new link handler
addLinkHandler :: LinkInfoCfg
               -> (URI -> Bool)
               -- ^Predicate function.
               -> (URI -> IO (LinkInfo Text))
               -- ^Link info function, applied if the predicate matches.
               -> LinkInfoCfg
addLinkHandler lic p h = LIC { _numLinks     = _numLinks lic
                             , _maxTitleLen  = _maxTitleLen lic
                             -- Add in reverse order so plugins added earlier override ones added later
                             , _linkHandlers = _linkHandlers lic ++ [(p, h)]
                             }

-- |Find the handler for a URI.
getLinkHandler :: LinkInfoCfg -> URI -> Maybe (URI -> IO (LinkInfo Text))
getLinkHandler lic uri = listToMaybe . map snd . filter (($uri) . fst) . _linkHandlers $ lic

-- |Turn a function producing a Maybe title into a function producing
-- a LinkTitle.
liftHandler :: MonadIO m => (URI -> m (Maybe Text)) -> URI -> m (LinkInfo Text)
liftHandler f uri = do
  title <- f uri
  return $
    case strip <$> title of
      Just "" -> NoTitle
      Just t  -> Title t
      Nothing -> Failed

-- *Title fetching

-- |Try to fetch the title of a URL.
fetchTitle :: MonadIO m => URI -> m (Maybe Text)
fetchTitle uri = liftIO $ do
  downloaded <- fetchHtml uri
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

