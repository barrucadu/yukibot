{-# LANGUAGE OverloadedStrings #-}

-- |Fetch and display information about links in messages.
module Yukibot.Plugins.LinkInfo (eventHandler) where

import Control.Applicative        ((<$>))
import Control.Monad.IO.Class     (MonadIO, liftIO)
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

eventHandler :: AsakuraEventHandler
eventHandler = AsakuraEventHandler
                 { _description = pack "Display information on links which come up in chat."
                 , _matchType   = EPrivmsg
                 , _eventFunc   = eventFunc
                 , _appliesTo   = runEverywhere
                 , _appliesDef  = runAlways
                 }

-- |Split a message up into words, and display information on the
-- first 5 links.
--
-- TODO: Make the limit configurable.
eventFunc :: IRCState -> Event -> Bot (IRC ())
eventFunc _ ev = return $ do
  let Privmsg msg = _message ev
  let urls = filter (isPrefixOf $ pack "http") . T.words $ msg

  responses <- catMaybes . zipWith showTitle urls <$> mapM fetchTitle urls

  case _source ev of
    Channel _ c -> mapM_ (send . privmsg c) $ take 5 responses
    User n      -> mapM_ (send . query n)   $ take 5 responses
    _           -> return ()

  where showTitle _ ""      = Nothing
        showTitle url title = Just $  "\"" <> title <> "\" [" <> url <> "]"

-- |Try to fetch the title of a URL.
fetchTitle :: MonadIO m => Text -> m Text
fetchTitle url = liftIO $ do
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
