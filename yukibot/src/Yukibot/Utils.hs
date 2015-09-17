{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

-- |Common utility functions for plugins.
module Yukibot.Utils where

import Control.Arrow ((***), second)
import Control.Exception (catch)
import Control.Lens ((&), (.~), (^.), (?~), (^?), ix)
import Control.Monad (guard)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Reader (ask)
import Data.Aeson (FromJSON, Object, Value(Object), decode', encode)
import Data.Aeson.Lens (_String)
import Data.Aeson.Types (emptyObject)
import Data.ByteString (ByteString, isInfixOf)
import Data.ByteString.Lazy (toStrict, fromStrict)
import Data.Char (isDigit)
import Data.List (groupBy)
import Data.Function (on)
import Data.Maybe (fromMaybe, fromJust)
import Data.Monoid ((<>))
import Data.String (IsString(..))
import Data.Text (Text, strip, unpack, pack, breakOn)
import Data.Text.Encoding (decodeUtf8)
import Data.Time.Clock (NominalDiffTime, UTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeOrError)
import Database.MongoDB (Action, Collection, Document, Label, Order, Selector, Val, access, close, connect, delete, host, master, find, rest, select, sort)
import Network.IRC.Asakura.Types (Bot, BotState(_config))
import Network.IRC.Client.Types (Event(_source), UnicodeEvent, Source(..))
import Network.HTTP.Client (HttpException)
import Network.Wreq (FormParam(..), Options, Response, auth, basicAuth, defaults, getWith, post, redirects, responseBody, responseHeader, responseStatus, statusCode)
import Network.URI (URI(..), URIAuth(..), uriToString)
import Text.Read (readMaybe)

import qualified Data.HashMap.Strict as HM
import qualified Data.Text           as T
import qualified Database.MongoDB    as Mo

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
fetchHtmlWithCreds url user pass = fetchHtml' url opts where
  opts = defaults & auth ?~ basicAuth (fromString user) (fromString pass)

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
fetchHttp' url opts = liftIO $ fetch `catch` handler where
  fetch = do
    res <- getWith (opts & redirects .~ 10) $ showUri url

    return $ if res ^. responseStatus . statusCode == 200
             then Just $ toStrict <$> res
             else Nothing

  handler = const $ return Nothing :: HttpException -> IO (Maybe (Response ByteString))

-- |Convert a URI into a string-like thing.
showUri :: IsString s => URI -> s
showUri uri = fromString $ uriToString id uri ""

-- |Construct a URI
makeUri ::
    String
  -- ^The domain
  -> String
  -- ^The path
  -> Maybe String
  -- ^The query string
  -> URI
makeUri domain path query = URI
  { uriScheme    = "http:"
  , uriAuthority = Just URIAuth
    { uriUserInfo = ""
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
epoch = parseTimeOrError True defaultTimeLocale "%s" "0"

-- |Display UTC in a format "HH:MM (YYYY-MM-DD)
showUtc :: UTCTime -> Text
showUtc = pack . formatTime defaultTimeLocale "%R (%F)"

-- | Intrepret an ISO 8601 duration string.
iso8601Duration :: Text -> Maybe NominalDiffTime
iso8601Duration = fmap fromIntegral . period . groupBy ((==) `on` isDigit) . unpack where
  period :: [String] -> Maybe Int
  period ("P":ps)  = period ps
  period ("PT":ts) = time ts
  period (ys:"Y":ps)  = (\y r -> r + y * 31536000) <$> readMaybe ys <*> period ps
  period (ys:"YT":ts) = (\y r -> r + y * 31536000) <$> readMaybe ys <*> time   ts
  period (ms:"M":ps)  = (\m r -> r + m * 2628000)  <$> readMaybe ms <*> period ps
  period (ms:"MT":ts) = (\m r -> r + m * 2628000)  <$> readMaybe ms <*> time   ts
  period (ws:"W":ps)  = (\w r -> r + w * 604800)   <$> readMaybe ws <*> period ps
  period (ws:"WT":ts) = (\w r -> r + w * 605800)   <$> readMaybe ws <*> time   ts
  period (ds:"D":ps)  = (\d r -> r + d * 86400)    <$> readMaybe ds <*> period ps
  period (ds:"DT":ts) = (\d r -> r + d * 86400)    <$> readMaybe ds <*> time   ts
  period [] = Just 0
  period _  = Nothing

  time (hs:"H":ts) = (\h r -> r + h * 3600) <$> readMaybe hs <*> time ts
  time (ms:"M":ts) = (\m r -> r + m * 60)   <$> readMaybe ms <*> time ts
  time (ss:"S":[]) = readMaybe ss
  time [] = Just 0
  time _  = Nothing

-- | Show a duration nicely
showDuration :: NominalDiffTime -> String
showDuration dur = sign ++ hours ++ mins ++ ":" ++ secs where
  sign = if dur < 0 then "-" else ""

  (hrs, dur') = abs (round dur) `quotRem` 3600
  (ms,  ss)   = dur' `quotRem` 60

  hours = if hrs /= 0 then show hrs ++ ":" else ""
  mins  = if ms < 10 then "0" ++ show ms else show ms
  secs  = if ss < 10 then "0" ++ show ss else show ss

-- *Embedded configuration

-- |Extract a value from the embedded JSON
--
-- TODO: Got to be a more efficient way of doing this.
cfgGet :: FromJSON a => Text -> Bot (Maybe a)
cfgGet name = do
  json <- _config <$> ask
  let (Object thing) = json
  let obj = HM.lookupDefault emptyObject name thing
  return . decode' $ encode obj

-- |Extract a value from the embedded JSON with a default value.
cfgGet' :: FromJSON a => a -> Text -> Bot a
cfgGet' d name = fromMaybe d <$> cfgGet name

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

-- | Zip two sorted lists into pairs, dropping the lesser element when
-- pairs do not match.
pairs :: (a -> b -> Bool) -> (a -> b -> Ordering) -> [a] -> [b] -> [(a,b)]
pairs eq cmp = pairs' where
  pairs' _ [] = []
  pairs' [] _ = []
  pairs' (a:as) (b:bs)
    | a `eq`  b      = (a,b) : pairs' as bs
    | a `cmp` b == GT = pairs' (a:as) bs
    | otherwise      = pairs' as (b:bs)

-- | Apply a function to both elements of a tuple.
(^*^) :: (a -> b) -> (a, a) -> (b, b)
f ^*^ x = (f *** f) x

-- | Like breakOn, but strip the delimiter.
breakOn' :: Text -> Text -> (Text, Text)
breakOn' delim txt = second (T.drop 1) (breakOn delim txt)

-- | Get the nick which sent a message
sender :: UnicodeEvent -> Maybe Text
sender ev = case _source ev of
  User nick      -> Just nick
  Channel _ nick -> Just nick
  Server _       -> Nothing

-- | Get the nick which sent a message, throwing an error if there was
-- no such nick.
sender' :: UnicodeEvent -> Text
sender' = fromJust . sender

-- | Show a number, with commas.
showNum :: (Integral i, Show i) => i -> String
showNum = reverse . go . reverse . show where
  go xs = case splitAt 3 xs of
    (first, [])   -> first
    (first, rest) -> first ++ "," ++ go rest
