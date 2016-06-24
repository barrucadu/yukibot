{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Yukibot.Backend.IRC.Configuration
-- Copyright   : (2) 2016 Michael Walker
-- License     : MIT
-- Stability   : experimental
-- Portability : LambdaCase, OverloadedStrings
--
-- Configuration for the yukibot-core IRC backend.
module Yukibot.Backend.IRC.Configuration
  ( -- * Validation
    ConfigurationError(..)
  , checkConfig
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

-- | An error in the configuration
data ConfigurationError
  = MissingNick -- ^ The nick must be present and nonempty.
  | MissingPort -- ^ The port must be present and >0.
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

-- | Check that the configuration is all ok, and return a description
-- if so.
checkConfig :: Text -> Table -> Either ConfigurationError Text
checkConfig host cfg = case (mgetNick cfg, mgetPort cfg) of
  (Just n, Just p) | T.length n > 0 && p > 0 -> Right $
    let tls = if getTLS cfg then "ssl://" else ""
        port = pack (show p)
    in "IRC <" <> tls <> n <> "@" <> host <> ":" <> port <> ">"
  (Just _, _)  -> Left MissingNick
  (_, Just _)  -> Left MissingPort
  (Nothing, _) -> Left MissingNick

  -- You might expect this case here:
  --
  -- @(_, Nothing) -> Left MissingPort@
  --
  -- But this overlaps things above, so it's not necessary. The effect
  -- of this is that nick errors take priority, in effect, over port
  -- errors.

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
