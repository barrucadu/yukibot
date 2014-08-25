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
import Control.Monad              (liftM)
import Control.Monad.IO.Class     (MonadIO, liftIO)
import Data.Maybe                 (mapMaybe)
import Data.Monoid                ((<>))
import Data.Text                  (Text, pack, unpack)
import Network.IRC.Asakura.Events (runAlways, runEverywhere)
import Network.IRC.Asakura.Types  (AsakuraEventHandler(..), Bot)
import Network.IRC.IDTE           (reply)
import Network.URI                (URI, parseURI)
import Network.IRC.IDTE.Types     (Event(..), EventType(EPrivmsg), IRC, IrcMessage(..), IRCState)
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
--
-- TODO: Don't show titles where URL is too similar.
--
-- TODO: Steal special titles from Mathison.
eventFunc :: LinkInfoCfg -> IRCState -> Event -> Bot (IRC ())
eventFunc cfg _ ev = return $ do
  let Privmsg msg = _message ev
  let urls = mapMaybe (parseURI . unpack) . T.words $ msg

  responses <- take (_numLinks cfg) . zipWith showTitle urls <$> mapM (fetchLinkInfo cfg) urls

  mapM_ (reply ev) responses

  where showTitle url (Just title) = "\"" <> title <> "\" [" <> showUri url <> "]"
        showTitle url Nothing      = "Could not retrieve title for " <> showUri url

-- *External usage

-- |Try to fetch information on a URL. If there is no specific
-- handler, this will just be the truncated title.
fetchLinkInfo :: MonadIO m => LinkInfoCfg -> URI -> m (Maybe Text)
fetchLinkInfo cfg url = case getLinkHandler cfg url of
                          Just handler -> liftIO $ handler url
                          Nothing      -> liftM (fmap trunc) $ fetchTitle url
    where trunc txt | T.length txt > _maxTitleLen cfg = T.take (_maxTitleLen cfg - 1) txt <> "…"
          trunc txt = txt
