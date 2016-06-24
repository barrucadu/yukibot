{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Yukibot.Backend.IRC.Configuration
-- Copyright   : (c) 2016 Michael Walker
-- License     : MIT
-- Stability   : experimental
-- Portability : LambdaCase, OverloadedStrings
--
-- Configuration for the yukibot-core IRC backend.
module Yukibot.Backend.IRC.Configuration
  ( -- * Validation
    checkConfig
  , mgetNick
  , mgetPort

  -- * Accessors
  , getNick
  , getPort
  , getTLS
  , getChannels
  , getServerPassword
  , getNickserv
  , getNickservPassword

  -- * Re-exports
  , Table
  ) where

import Data.Maybe (fromMaybe, mapMaybe)
import Data.Monoid ((<>))
import Data.Text (Text, pack)
import qualified Data.Text as T

import Yukibot.Configuration

-- | Check that the configuration is all ok, and return either an
-- error message, or a description.
checkConfig :: Text -> Table -> Either Text Text
checkConfig host cfg = case (mgetNick cfg, mgetPort cfg) of
  (Just n, Just p) | T.length n > 0 && p > 0 -> Right $
    let tls = if getTLS cfg then "ssl://" else ""
        port = pack (show p)
    in "IRC <" <> tls <> n <> "@" <> host <> ":" <> port <> ">"
  (Just n, _)  | T.length n <= 0 -> Left "nick must be longer than 0 characters"
  (_, Just p)  | p <= 0          -> Left "port must be greater than 0"
  (Nothing, _) -> Left "nick is required"
  _            -> Left "port is required"

-- | Get the nick from the configuration.
--
-- A valid configuration (i.e., one for which 'checkConfig' returns
-- @Right@) returns @Just@ here.
mgetNick :: Table -> Maybe Text
mgetNick = getString "nick"

-- | Get the port from the configuration.
--
-- A valid configuration (i.e., one for which 'checkConfig' returns
-- @Right@) returns @Just@ here.
mgetPort :: Table -> Maybe Int
mgetPort = getInteger "port"

-------------------------------------------------------------------------------

-- | Get the nick from the configuation. Calls 'error' if missing.
getNick :: Table -> Text
getNick = fromMaybe (error "Missing nick!") . mgetNick

-- | Get the port from the configuration. Calls 'error' if missing.
getPort :: Table -> Int
getPort = fromMaybe (error "Missing port!") . mgetPort

-- | Get the TLS flag from the configuration. Defaults to @False@.
getTLS :: Table -> Bool
getTLS = fromMaybe False . getBool "tls"

-- | Get the list of channels to join from the configuration. Defaults
-- to @[]@.
getChannels :: Table -> [Text]
getChannels = mapMaybe (\case { VString c -> Just c; _ -> Nothing }) . fromMaybe [] . getArray "channels"

-- | Get the server password from the configuration. Defaults to
-- @Nothing@.
getServerPassword :: Table -> Maybe Text
getServerPassword = mguard ((>0) . T.length) . getString "server-password"

-- | Get the nickserv nick from the configuration. Defaults to
-- @"nickserv"@.
getNickserv :: Table -> Text
getNickserv = fromMaybe "nickserv" . getString "nickserv"

-- | Get the nickserv password from the configuration. Defaults to
-- @Nothing@.
getNickservPassword :: Table -> Maybe Text
getNickservPassword = mguard ((>0) . T.length) . getString "nickserv-password"

-------------------------------------------------------------------------------

-- | Check if a maybe value meets some criteria.
mguard :: (a -> Bool) -> Maybe a -> Maybe a
mguard f ma@(Just a) | f a = ma
mguard _ _ = Nothing
