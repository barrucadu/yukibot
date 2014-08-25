-- |Common utility functions for plugins.
module Yukibot.Utils where

import Control.Arrow          ((***))
import Control.Monad          (guard)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Char              (toLower)
import Data.List              (isInfixOf)
import Network.Curl           (CurlBuffer, CurlOption(..), CurlResponse_(..), URLString, withCurlDo, curlGetResponse_)

-- |Download an HTML document. Return (Just html) if we get a 200
-- response code, and a html-y content-type. This follows redirects.
fetchHtml :: MonadIO m => URLString -> m (Maybe String)
fetchHtml = flip fetchHtml' []

-- |Like 'fetchHtml', but accepts a username and password.
fetchHtmlWithCreds :: MonadIO m => URLString -> String -> String -> m (Maybe String)
fetchHtmlWithCreds url user pass = fetchHtml' url [ CurlUserPwd $ user ++ ":" ++ pass ]

-- |Like 'fetchHtml', but takes options (in addition to following
-- redirects).
fetchHtml' :: MonadIO m => URLString -> [CurlOption] -> m (Maybe String)
fetchHtml' url opts = do
  res <- fetchHttp' url opts

  return $ do
    response <- res

    guard $ case lookup "content-type" $ map (map toLower *** map toLower) $ respHeaders response of
              Just ctype -> "html" `isInfixOf` ctype
              Nothing    -> False

    Just $ respBody response

-- |Download something over HTTP, returning (Just response) on a 200
-- response code. This follows redirects.
fetchHttp :: (CurlBuffer c, MonadIO m) => URLString -> m (Maybe (CurlResponse_ [(String, String)] c))
fetchHttp = flip fetchHttp' []

-- |Like 'fetchHttp', but also takes options (in addition to following
-- redirects).
fetchHttp' :: (CurlBuffer c, MonadIO m) => URLString -> [CurlOption] -> m (Maybe (CurlResponse_ [(String, String)] c))
fetchHttp' url opts = liftIO . withCurlDo $ do
  res <- curlGetResponse_ url $ CurlFollowLocation True : opts

  return $ if respStatus res == 200
           then Just res
           else Nothing
