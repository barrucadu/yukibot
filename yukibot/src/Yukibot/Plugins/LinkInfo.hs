{-# LANGUAGE OverloadedStrings #-}

-- |Fetch and display information about links in messages.
module Yukibot.Plugins.LinkInfo
  (-- *Configuration
   LinkInfoCfg
  , defaultLinkInfoCfg
  -- *Event handler
  , eventHandler
  -- *External usage
  , fetchLinkInfo
  , fetchTitle
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (FromJSON(..), ToJSON(..), Value(..), (.=), (.:?), (.!=), object)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Monoid ((<>))
import Data.Text (Text, pack, unpack, isPrefixOf, toLower, strip)
import Network.IRC.Bot.Events (runAlways, runEverywhere)
import Network.IRC.Bot.Types (EventHandler(..), Bot)
import Network.IRC.Client (reply)
import Network.IRC.Client.Types (Event(..), EventType(EPrivmsg), IRC, Message(..), UnicodeEvent, IRCState)
import Network.URI (URI, parseURI)

import Yukibot.Plugins.LinkInfo.Common
import Yukibot.Plugins.LinkInfo.Imgur
import Yukibot.Plugins.LinkInfo.PageTitle
import Yukibot.Plugins.LinkInfo.Soundcloud
import Yukibot.Plugins.LinkInfo.Youtube
import Yukibot.Utils

import qualified Data.Text as T

-- *State
--
-- This is handled here to avoid cyclic module imports.

instance ToJSON LinkInfoCfg where
  toJSON cfg = object $ catMaybes
    [ Just $ "numLinks" .= _numLinks    cfg
    , Just $ "maxLen"   .= _maxTitleLen cfg
    , Just $ "handlers" .= map _licName (_linkHandlers cfg)
    , ("soundcloud" .=) <$> _soundcloud cfg
    , ("youtube"    .=) <$> _youtube cfg
    ]

instance FromJSON LinkInfoCfg where
  parseJSON (Object v) = do
    numLinks   <- v .:? "numLinks" .!= _numLinks defaultLinkInfoCfg
    maxLen     <- v .:? "maxLen"   .!= _maxTitleLen defaultLinkInfoCfg
    handlers   <- v .:? "handlers"
    soundcloud <- v .:? "soundcloud"
    youtube    <- v .:? "youtube"

    let handlers' = case handlers of
          Just hs -> populateHandlers maxLen soundcloud youtube hs
          Nothing -> _linkHandlers defaultLinkInfoCfg

    return $ LIC numLinks maxLen handlers' soundcloud youtube

  parseJSON _ = fail "Expected object"

-- | Limit of 5 links, title limit of 100 characters, using imgur and
-- page title only.
defaultLinkInfoCfg :: LinkInfoCfg
defaultLinkInfoCfg = LIC
  { _numLinks     = 5
  , _maxTitleLen  = 100
  , _linkHandlers = [imgurLinks, pageTitle $ _maxTitleLen defaultLinkInfoCfg]
  , _soundcloud   = Nothing
  , _youtube      = Nothing
  }

-- *Event handler

eventHandler :: LinkInfoCfg -> EventHandler
eventHandler cfg = EventHandler
  { _description = pack "Display information on links which come up in chat."
  , _matchType   = EPrivmsg
  , _eventFunc   = eventFunc cfg
  , _appliesTo   = runEverywhere
  , _appliesDef  = runAlways
  }

-- |Split a message up into words, and display information on the
-- first `numLinks` links.
eventFunc :: LinkInfoCfg -> IRCState -> UnicodeEvent -> Bot (IRC ())
eventFunc cfg _ ev = return $ do
  let Privmsg _ (Right msg) = _message ev
  let urls = mapMaybe toUri . T.words $ msg

  responses <- take (_numLinks cfg) . catMaybes . zipWith showTitle urls <$> mapM (fetchLinkInfo cfg) urls

  mapM_ (reply ev) responses

  where toUri uri | "http" `isPrefixOf` uri = parseURI $ unpack uri
                  | otherwise = Nothing

        showTitle _   (Title title) = Just $ "Title: \"" <> title <> "\""
        showTitle _   (Info info)   = Just info
        showTitle _   NoTitle       = Nothing
        showTitle url Failed        = Just $ "Could not retrieve title for " <> showUri url

-- *External usage

-- |Try to fetch information on a URL.
fetchLinkInfo :: MonadIO m => LinkInfoCfg -> URI -> m (LinkInfo Text)
fetchLinkInfo cfg url = liftIO $ unempty <$> handler url where
  -- Get the handler, or a fallback if none exists
  handler = maybe (\_ -> return NoTitle) _licHandler $ getLinkHandler cfg url

  -- Strip out empty titles
  unempty (Title t) | strip t == "" = NoTitle
  unempty (Info  i) | strip i == "" = NoTitle
  unempty t = t

-- *Helpers

-- |Turn a list of names of handlers into a list of handlers.
populateHandlers :: Int
                 -> Maybe Text
                 -- ^ Soundcloud API key
                 -> Maybe Text
                 -- ^ Youtube API key
                 -> [Text] -> [LinkHandler]
populateHandlers maxlen soundcloud youtube = mapMaybe (toHandler . toLower) where
  toHandler "imgur"      = Just imgurLinks
  toHandler "youtube"    = Just $ youtubeLinks youtube
  toHandler "soundcloud" = Just $ soundcloudLinks soundcloud
  toHandler "pagetitle"  = Just $ pageTitle maxlen
  toHandler _ = Nothing
