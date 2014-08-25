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
import Data.Monoid                ((<>))
import Data.Text                  (Text, isPrefixOf, pack)
import Network.IRC.Asakura.Events (runAlways, runEverywhere)
import Network.IRC.Asakura.Types  (AsakuraEventHandler(..), Bot)
import Network.IRC.IDTE           (privmsg, query, send)
import Network.IRC.IDTE.Types     (Event(..), EventType(EPrivmsg), IRC, IrcMessage(..), IRCState, Source(..))
import Yukibot.Plugins.LinkInfo.Common

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
  let urls = filter (isPrefixOf $ pack "http") . T.words $ msg

  responses <- take (_numLinks cfg) . zipWith showTitle urls <$> mapM (fetchLinkInfo cfg) urls

  case _source ev of
    Channel _ c -> mapM_ (send . privmsg c) responses
    User n      -> mapM_ (send . query n)   responses
    _           -> return ()

  where showTitle url (Just title) = "\"" <> title <> "\" [" <> url <> "]"
        showTitle url Nothing      = "Could not retrieve title for " <> url

-- *External usage

-- |Try to fetch information on a URL. If there is no specific
-- handler, this will just be the truncated title.
fetchLinkInfo :: MonadIO m => LinkInfoCfg -> Text -> m (Maybe Text)
fetchLinkInfo cfg url = case getLinkHandler cfg url of
                          Just handler -> liftIO $ handler url
                          Nothing      -> liftM (fmap trunc) $ fetchTitle url
    where trunc txt | T.length txt > _maxTitleLen cfg = T.take (_maxTitleLen cfg - 1) txt <> "…"
          trunc txt = txt
