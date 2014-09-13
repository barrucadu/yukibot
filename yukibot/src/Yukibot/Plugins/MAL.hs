{-# LANGUAGE Arrows            #-}
{-# LANGUAGE OverloadedStrings #-}

-- |A MyAnimeList scraper. Requires credentials to be set in the
-- config file.
module Yukibot.Plugins.MAL
    ( -- *Configration
      MALCfg
    -- *Command
    , malCommand
    -- *External usage
    , malQuery
    ) where

import Control.Applicative       ((<$>), (<*>))
import Control.Monad.IO.Class    (MonadIO, liftIO)
import Data.Aeson                (FromJSON(..), ToJSON(..), Value(..), (.=), (.:?), (.!=), object)
import Data.Default.Class        (Default(..))
import Data.Monoid               ((<>))
import Data.Text                 (Text, pack, unpack, intercalate)
import Network.IRC.Asakura.Types (Bot)
import Network.IRC.Client        (reply)
import Network.IRC.Client.Types  (IRC, IRCState, UnicodeEvent)
import Text.XML.HXT.Core
import Network.URI               (escapeURIString, isAllowedInURI)
import Yukibot.Utils             (fetchHtmlWithCreds, makeUri)

import qualified Data.Text as T

-- *Configuration

-- |The MAL API accepts credentials over HTTP auth.
data MALCfg = MC
    { _username :: String
    , _password :: String
    , _limit    :: Int
    }

instance ToJSON MALCfg where
    toJSON mc = object [ "username" .= toJSON (_username mc)
                       , "password" .= toJSON (_password mc)
                       , "limit"    .= toJSON (_limit mc)
                       ]

instance FromJSON MALCfg where
    parseJSON (Object v) = MC <$> v .:? "username" .!= _username def
                              <*> v .:? "password" .!= _password def
                              <*> v .:? "limit"    .!= _limit    def
    parseJSON _ = fail "Expected object"

instance Default MALCfg where
    def = MC "" "" 5

-- *Command

-- |Interpret everything after the command as an anime search term,
-- and query MAL for it.
malCommand :: MALCfg -> [Text] -> IRCState -> UnicodeEvent -> Bot (IRC ())
malCommand mc args _ ev = return $ do
  let term = intercalate "+" args
  res <- malQuery mc term

  case res of
    Just result -> mapM_ (reply ev) result
    Nothing     -> reply ev $ "Failed to get information on " <> term

-- *External usage

-- |Use the MAL search API to look up an anime.
malQuery :: MonadIO m => MALCfg -> Text -> m (Maybe [Text])
malQuery mc term = liftIO $ do
  let uri = makeUri "myanimelist.net" "/api/anime/search.xml" . Just $ "q=" <> escapeURIString isAllowedInURI (unpack term)
  downloaded <- fetchHtmlWithCreds uri (_username mc) $ _password mc

  case downloaded of
    Just xml -> do
      let doc = readString [ withSubstDTDEntities  no
                           , withSubstHTMLEntities yes
                           , withValidate no
                           , withWarnings no ] xml

      res <- runX $ doc //> atTag "entry" >>> parseEntry

      return . Just . map toShowInfo $ take (_limit mc) res

    _ -> return Nothing

-- |Turn a show info tuple into a textual description
toShowInfo :: (Text, Text, Text, Text, Text) -> Text
toShowInfo (id_, title, episodes, type_, status) = T.unwords [ typeInfo
                                                             , title
                                                             , link
                                                             , currentlyAiring
                                                             ]

    where typeInfo = case (episodes, type_) of
                       ("1", _)     -> "[" <> type_ <> "]"
                       (_, "Movie") -> "[" <> type_ <> "]"
                       _            -> "[" <> type_ <> " " <> episodes <> " eps]"

          link = "[http://myanimelist.net/anime/" <> id_ <> "]"

          currentlyAiring | status == "Currently Airing" = "~Currently Airing~"
                          | otherwise = ""

-- |Parse an <entry> into a tuple.
parseEntry :: ArrowXml a => a XmlTree (Text, Text, Text, Text, Text)
parseEntry = proc entry -> do
  id_      <- childText "id"       -< entry
  title    <- childText "title"    -< entry
  episodes <- childText "episodes" -< entry
  type_    <- childText "type"     -< entry
  status   <- childText "status"   -< entry

  returnA -< ( pack id_
            , pack title
            , pack episodes
            , pack type_
            , pack status
            )

-- |Run an arrow on a named tag
atTag :: ArrowXml a => String -> a XmlTree XmlTree
atTag tag = isElem >>> hasName tag

-- |Extract the text of a named child tag
childText :: ArrowXml a => String -> a XmlTree String
childText tag = getChildren >>> atTag tag /> getText
