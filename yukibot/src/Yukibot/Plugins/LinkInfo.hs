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
import Data.Maybe                 (catMaybes, mapMaybe)
import Data.Monoid                ((<>))
import Data.Text                  (Text, pack, unpack, isPrefixOf)
import Network.IRC.Asakura.Events (runAlways, runEverywhere)
import Network.IRC.Asakura.Types  (AsakuraEventHandler(..), Bot)
import Network.IRC.Client         (reply)
import Network.IRC.Client.Types   (Event(..), EventType(EPrivmsg), IRC, Message(..), UnicodeEvent, IRCState)
import Network.URI                (URI, parseURI)
import Yukibot.Plugins.LinkInfo.Common
import Yukibot.Utils              (showUri)

import qualified Data.Text as T

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
--
-- TODO: Don't display empty titles
fetchLinkInfo :: MonadIO m => LinkInfoCfg -> URI -> m (LinkInfo Text)
fetchLinkInfo cfg url = liftIO . maybe (return NoTitle) ($ url) $ getLinkHandler cfg url
