{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

-- |Common utility functions for plugins.
module Yukibot.Utils where

import Control.Applicative    ((<$>))
import Control.Exception      (catch)
import Control.Lens           ((&), (.~), (^.), (?~), (^?), ix)
import Control.Monad          (guard)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Reader (ask)
import Data.Aeson             (FromJSON, Object, Value, decode', encode)
import Data.Aeson.Lens        (_String)
import Data.ByteString        (ByteString, isInfixOf)
import Data.ByteString.Lazy   (toStrict, fromStrict)
import Data.Maybe             (fromMaybe)
import Data.Monoid            ((<>))
import Data.String            (IsString(..))
import Data.Text              (Text, strip, unpack, pack)
import Data.Text.Encoding     (decodeUtf8)
import Data.Time.Clock        (UTCTime)
import Data.Time.Format       (formatTime, readTime)
import Database.MongoDB       (Action, Collection, Document, Label, Order, Selector, Val, access, close, connect, delete, host, master, find, rest, select, sort)
import Network.IRC.Asakura.Types (Bot, BotState(_config))
import Network.HTTP.Client    (HttpException)
import Network.Wreq           (FormParam(..), Options, Response, auth, basicAuth, defaults, getWith, post, redirects, responseBody, responseHeader, responseStatus, statusCode)
import Network.URI            (URI(..), URIAuth(..), uriToString)
import System.Locale          (defaultTimeLocale)

import qualified Database.MongoDB as Mo

-- *Webby Stuff

-- |Download an HTML document. Return (Just html) if we get a 200
-- response code, and a html-y content-type. This follows redirects.
--
-- This decodes the result as UTF-8. If another encoding is desired,
-- use 'fetchHttp' directly.
fetchHtml :: MonadIO m => URI -> m (Maybe String)
fetchHtml = flip fetchHtml' defaults

-- |Like 'fetchHtml', but accepts a username and password.
--
-- This decodes the result as UTF-8. If another encoding is desired,
-- use 'fetchHttp' directly.
fetchHtmlWithCreds :: MonadIO m => URI -> String -> String -> m (Maybe String)
fetchHtmlWithCreds url user pass = fetchHtml' url opts
    where opts = defaults & auth ?~ basicAuth (fromString user) (fromString pass)

-- |Like 'fetchHtml', but takes options (in addition to following
-- redirects).
--
-- This decodes the result as UTF-8. If another encoding is desired,
-- use 'fetchHttp' directly.
fetchHtml' :: MonadIO m => URI -> Options -> m (Maybe String)
fetchHtml' url opts = do
  res <- fetchHttp' url opts

  return $ do
    response <- res

    guard $ "html" `isInfixOf` (response ^. responseHeader "Content-Type")

    Just . unpack . decodeUtf8 $ response ^. responseBody

-- |Download some JSON over HTTP.
fetchJson :: MonadIO m => URI -> m (Maybe Object)
fetchJson uri = do
  res <- fetchHttp uri

  -- Attempt to JSON
  return $ do
    response <- res
    decode' . fromStrict $ response ^. responseBody

-- |Download something over HTTP, returning (Just response) on a 200
-- response code. This follows redirects.
fetchHttp :: MonadIO m => URI -> m (Maybe (Response ByteString))
fetchHttp = flip fetchHttp' defaults

-- |Like 'fetchHttp', but also takes options (in addition to following
-- redirects).
fetchHttp' :: MonadIO m => URI -> Options -> m (Maybe (Response ByteString))
fetchHttp' url opts = liftIO $ fetch `catch` handler
    where fetch = do
            res <- getWith (opts & redirects .~ 10) $ showUri url

            return $ if res ^. responseStatus . statusCode == 200
                     then Just $ toStrict <$> res
                     else Nothing

          handler = const $ return Nothing :: HttpException -> IO (Maybe (Response ByteString))

-- |Convert a URI into a string-like thing.
showUri :: IsString s => URI -> s
showUri uri = fromString $ uriToString id uri ""

-- |Construct a URI
makeUri :: String
        -- ^The domain
        -> String
        -- ^The path
        -> Maybe String
        -- ^The query string
        -> URI
makeUri domain path query = URI { uriScheme    = "http:"
                                , uriAuthority = Just URIAuth { uriUserInfo = ""
                                                              , uriRegName  = domain
                                                              , uriPort     = ""
                                                              }
                                , uriPath      = path
                                , uriQuery     = fromMaybe "" query
                                , uriFragment  = ""
                                }

-- |Upload some text to sprunge and return the response body (the URI)
paste :: MonadIO m => String -> m Text
paste txt = do
  r <- liftIO $ post "http://sprunge.us" [ "sprunge" := txt ]
  return . strip . decodeUtf8 . toStrict $ r ^. responseBody

-- *MongoDB

-- |Type to encapsulate the MongoDB connection info
newtype Mongo = Mongo (String, Collection)

-- |A namespace, prepended to collection names passed into the
-- 'defaultMongo' functions.
mongoNamespace :: Collection
mongoNamespace = "yukibot__"

-- |Construct a 'Mongo' using the global host, or localhost if not
-- found.
defaultMongo :: Collection -> Bot Mongo
defaultMongo c = flip defaultMongo' c . _config <$> ask

-- |Like 'defaultMongo', but use an explicit config.
defaultMongo' :: Value -> Collection -> Mongo
defaultMongo' cfg c = Mongo (unpack . fromMaybe "localhost" $ cfg ^? ix "mongodb" . ix "host" . _String, mongoNamespace <> c)

-- |Run a function over a MongoDB database
doMongo :: MonadIO m => Mongo -> (Collection -> Action m a) -> m a
doMongo (Mongo (h, c)) af = do
  pipe <- liftIO $ connect $ host h
  res <- access pipe master c $ af c
  liftIO $ close pipe
  return res

-- |Lookup values in a MongoDB database
queryMongo :: MonadIO m => Mongo -> Selector -> Order -> m [Document]
queryMongo mongo selby sortby = liftIO . doMongo mongo $ \c -> rest =<< find (select selby c) { sort = sortby }

-- |Delet values from a MongoDB database
deleteMongo :: MonadIO m => Mongo -> Selector -> m ()
deleteMongo mongo selby = liftIO . doMongo mongo $ \c -> delete (select selby c)

-- |Typed value of field in document, with a default for the
-- missing/bad type case.
at' :: Val v => Label -> v -> Document -> v
at' lbl def doc = def `fromMaybe` Mo.lookup lbl doc

-- *Time

-- |The UNIX epoch, a sane default time.
epoch :: UTCTime
epoch = readTime defaultTimeLocale "%s" "0"

-- |Display UTC in a format "HH:MM (YYYY-MM-DD)
showUtc :: UTCTime -> Text
showUtc = pack . formatTime defaultTimeLocale "%R (%F)"

-- *Embedded configuration

-- |Extract a value from the embedded JSON
--
-- TODO: Got to be a more efficient way of doing this.
cfgGet :: FromJSON a => Bot (Maybe a)
cfgGet = decode' . encode . _config <$> ask

-- |Extract a value from the embedded JSON with a default value.
cfgGet' :: FromJSON a => a -> Bot a
cfgGet' d = fromMaybe d <$> cfgGet

-- *Misc

-- |Split a string by a character
--
-- Why isn't this built in?
wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =
  case dropWhile p s of
    "" -> []
    s' -> let (w, s'') = break p s'
         in w : wordsWhen p s''
