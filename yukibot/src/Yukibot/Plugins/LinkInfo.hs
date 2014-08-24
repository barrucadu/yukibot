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
import Data.Maybe                 (catMaybes)
import Data.Monoid                ((<>))
import Data.Text                  (Text, isPrefixOf, pack, unpack)
import Network.IRC.Asakura.Events (runAlways, runEverywhere)
import Network.IRC.Asakura.Types  (AsakuraEventHandler(..), Bot)
import Network.IRC.IDTE           (privmsg, query, send)
import Network.IRC.IDTE.Types     (Event(..), EventType(EPrivmsg), IRC, IrcMessage(..), IRCState, Source(..))
import Text.XML.HXT.Arrow.XmlOptions (a_redirect)
import Text.XML.HXT.Core          ((//>), readDocument, hasName, getText, runX, withParseHTML, withWarnings, no, yes)
import Text.XML.HXT.Curl          (withCurl)
import Text.XML.HXT.TagSoup       (withTagSoup)

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

  responses <- take (_numLinks cfg) . catMaybes . zipWith showTitle urls <$> mapM (fetchTitle cfg) urls

  case _source ev of
    Channel _ c -> mapM_ (send . privmsg c) responses
    User n      -> mapM_ (send . query n)   responses
    _           -> return ()

  where showTitle _ ""      = Nothing
        showTitle url title = Just $  "\"" <> title <> "\" [" <> url <> "]"

-- *External usage

-- |Try to fetch the title of a URL.
--
-- Truncate titles longer than `maxLen`.
fetchTitle :: MonadIO m => LinkInfoCfg -> Text -> m Text
fetchTitle cfg url = liftM trunc $ fetchTitle' url
    where trunc txt | T.length txt > _maxTitleLen cfg = T.take (_maxTitleLen cfg - 1) txt <> "…"
          trunc txt = txt

-- |Try to fetch the title of a URL, with no length limit.
--
-- Note: an empty title is returned on failure - or if the title was
-- simply empty. I need to implement proper failure detection for
-- this.
fetchTitle' :: MonadIO m => Text -> m Text
fetchTitle' url = liftIO $ do
  let doc = readDocument [ withParseHTML      yes
                         , withTagSoup
                         , withCurl           [(a_redirect, "")]
                         , withWarnings       no
                         ] $ unpack url

  title <- runX $ doc //> hasName "title" //> getText
  return . pack . toPlainText . concat $ title

  where toPlainText = dequote . unwords . words
        dequote ('\"':xs) | last xs == '\"' = init xs
        dequote x = x
