{-# LANGUAGE OverloadedStrings #-}

-- |Fetch and display information about links in messages.
module Yukibot.Plugins.LinkInfo
    (-- *Configuration
     LinkInfoCfg
    -- *Event handler
    , eventHandler
    -- *External usage
    , fetchLinkInfo
    , fetchTitle
    ) where

import Control.Applicative        ((<$>))
import Control.Monad.IO.Class     (MonadIO, liftIO)
import Data.Aeson                 (FromJSON(..), ToJSON(..), Value(..), (.=), (.:?), (.!=), object)
import Data.Default.Class         (Default(..))
import Data.Maybe                 (catMaybes, mapMaybe)
import Data.Monoid                ((<>))
import Data.Text                  (Text, pack, unpack, isPrefixOf, toLower, strip)
import Network.IRC.Asakura.Events (runAlways, runEverywhere)
import Network.IRC.Asakura.Types  (AsakuraEventHandler(..), Bot)
import Network.IRC.Client         (reply)
import Network.IRC.Client.Types   (Event(..), EventType(EPrivmsg), IRC, Message(..), UnicodeEvent, IRCState)
import Network.URI                (URI, parseURI)
import Yukibot.Utils              (showUri)

import Yukibot.Plugins.LinkInfo.Common
import Yukibot.Plugins.LinkInfo.Imgur
import Yukibot.Plugins.LinkInfo.PageTitle
import Yukibot.Plugins.LinkInfo.Soundcloud
import Yukibot.Plugins.LinkInfo.Youtube

import qualified Data.Text as T

-- *State
--
-- This is handled here to avoid cyclic module imports.

instance ToJSON LinkInfoCfg where
    toJSON cfg = case _soundcloud cfg of
      Just apikey ->
        object [ "numLinks" .= _numLinks    cfg
               , "maxLen"   .= _maxTitleLen cfg
               , "handlers" .= map _licName (_linkHandlers cfg)
               , "soundcloud" .= apikey
               ]
      Nothing ->
        object [ "numLinks" .= _numLinks    cfg
               , "maxLen"   .= _maxTitleLen cfg
               , "handlers" .= map _licName (_linkHandlers cfg)
               ]

instance FromJSON LinkInfoCfg where
  parseJSON (Object v) = do
    numLinks   <- v .:? "numLinks" .!= _numLinks def
    maxLen     <- v .:? "maxLen"   .!= _maxTitleLen def
    handlers   <- v .:? "handlers"
    soundcloud <- v .:? "soundcloud"

    let handlers' = case handlers of
          Just hs -> populateHandlers maxLen soundcloud hs
          Nothing -> _linkHandlers def

    return $ LIC numLinks maxLen handlers' soundcloud

  parseJSON _ = fail "Expected object"

instance Default LinkInfoCfg where
    def = LIC { _numLinks     = 5
              , _maxTitleLen  = 100
              , _linkHandlers = [imgurLinks, youtubeLinks, pageTitle $ _maxTitleLen def]
              , _soundcloud   = Nothing
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
fetchLinkInfo cfg url = liftIO $ unempty <$> handler url
  where
    -- Get the handler, or a fallback if none exists
    handler = maybe (\_ -> return NoTitle) _licHandler $ getLinkHandler cfg url

    -- Strip out empty titles
    unempty (Title t) | strip t == "" = NoTitle
    unempty (Info  i) | strip i == "" = NoTitle
    unempty t = t

-- *Helpers

-- |Turn a list of names of handlers into a list of handlers.
populateHandlers :: Int -> Maybe Text -> [Text] -> [LinkHandler]
populateHandlers maxlen soundcloud = mapMaybe (toHandler . toLower)
  where
    toHandler "imgur"      = Just imgurLinks
    toHandler "youtube"    = Just youtubeLinks
    toHandler "soundcloud" = Just $ soundcloudLinks soundcloud
    toHandler "pagetitle"  = Just $ pageTitle maxlen
    toHandler _ = Nothing
