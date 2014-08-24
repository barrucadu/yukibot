{-# LANGUAGE OverloadedStrings #-}

-- |Fetch and display information about links in messages.
module Yukibot.Plugins.LinkInfo
    (-- *Configuration
     LinkInfoCfg
    -- *Event handler
    , eventHandler
    -- *External usage
    , fetchTitle
    , fetchTitle'
    ) where

import Control.Applicative        ((<$>), (<*>))
import Control.Monad              (liftM)
import Control.Monad.IO.Class     (MonadIO, liftIO)
import Data.Aeson                 (FromJSON(..), ToJSON(..), Value(..), (.=), (.:?), (.!=), object)
import Data.Default.Class         (Default(..))
import Data.Monoid                ((<>))
import Data.Text                  (Text, isPrefixOf, pack, unpack)
import Network.IRC.Asakura.Events (runAlways, runEverywhere)
import Network.IRC.Asakura.Types  (AsakuraEventHandler(..), Bot)
import Network.IRC.IDTE           (privmsg, query, send)
import Network.IRC.IDTE.Types     (Event(..), EventType(EPrivmsg), IRC, IrcMessage(..), IRCState, Source(..))
import Text.XML.HXT.Core          ((//>), readString, hasName, getText, runX, withParseHTML, withWarnings, no, yes)
import Text.XML.HXT.TagSoup       (withTagSoup)
import Yukibot.Utils              (fetchHtml)

import qualified Data.Text as T

-- *Configuration

-- |Currently configurable settings: number of link titles to show,
-- and length limit of titles.
data LinkInfoCfg = LIC
    { _numLinks    :: Int
    , _maxTitleLen :: Int
    }

instance ToJSON LinkInfoCfg where
    toJSON cfg = object [ "numLinks" .= _numLinks    cfg
                        , "maxLen"   .= _maxTitleLen cfg
                        ]

instance FromJSON LinkInfoCfg where
    parseJSON (Object v) = LIC <$> v .:? "numLinks" .!= _numLinks    def
                               <*> v .:? "maxLen"   .!= _maxTitleLen def
    parseJSON _ = fail "Expected object"

instance Default LinkInfoCfg where
    def = LIC { _numLinks    = 5
              , _maxTitleLen = 100
              }

-- *Event handler

eventHandler :: LinkInfoCfg -> AsakuraEventHandler
eventHandler cfg = AsakuraEventHandler
                   { _description = pack "Display information on links which come up in chat."
                   , _matchType   = EPrivmsg
                   , _eventFunc   = eventFunc cfg
                   , _appliesTo   = runEverywhere
                   , _appliesDef  = runAlways
                   }

-- |Split a message up into words, and display information on the
-- first `numLinks` links.
--
-- TODO: Don't show titles where URL is too similar.
--
-- TODO: Steal special titles from Mathison.
eventFunc :: LinkInfoCfg -> IRCState -> Event -> Bot (IRC ())
eventFunc cfg _ ev = return $ do
  let Privmsg msg = _message ev
  let urls = filter (isPrefixOf $ pack "http") . T.words $ msg

  responses <- take (_numLinks cfg) . zipWith showTitle urls <$> mapM (fetchTitle cfg) urls

  case _source ev of
    Channel _ c -> mapM_ (send . privmsg c) responses
    User n      -> mapM_ (send . query n)   responses
    _           -> return ()

  where showTitle url (Just title) = "\"" <> title <> "\" [" <> url <> "]"
        showTitle url Nothing      = "Could not retrieve title for " <> url

-- *External usage

-- |Try to fetch the title of a URL.
--
-- Truncate titles longer than `maxLen`.
fetchTitle :: MonadIO m => LinkInfoCfg -> Text -> m (Maybe Text)
fetchTitle cfg url = liftM (fmap trunc) $ fetchTitle' url
    where trunc txt | T.length txt > _maxTitleLen cfg = T.take (_maxTitleLen cfg - 1) txt <> "…"
          trunc txt = txt

-- |Try to fetch the title of a URL, with no length limit.
fetchTitle' :: MonadIO m => Text -> m (Maybe Text)
fetchTitle' url = liftIO $ do
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
