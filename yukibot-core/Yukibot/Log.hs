{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Yukibot.Log
-- Copyright   : (c) 2016 Michael Walker
-- License     : MIT
-- Stability   : experimental
-- Portability : OverloadedStrings
--
-- TODO: Time format configuration (format string in config file, or
-- enum?)
module Yukibot.Log
  ( -- * Backend logging
    Logger(..)
  , logger

  -- * Component loggers
  , Direction(..)
  , logRaw
  , logEvent
  , logAction
  ) where

import Data.Monoid ((<>))
import Data.Text (Text, unpack)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)

import Yukibot.Types

-- | Create a logger.
logger :: (channel -> Text) -> (user -> Text)
  -> FilePath -- ^ The raw log file
  -> FilePath -- ^ The event/action log file.
  -> Logger channel user
logger showc showu rawf unrawf = Logger
  { loggerToServer   = logRaw rawf ToServer
  , loggerFromServer = logRaw rawf FromServer
  , loggerEvent      = logEvent showc showu unrawf
  , loggerAction     = logAction showc showu unrawf
  }

-------------------------------------------------------------------------------
-- Component loggers

-- | Raw messages are those sent to and from the server, with no
-- interpretation.
logRaw :: FilePath -> Direction -> Text -> IO ()
logRaw fp direction msg = do
  now <- getCurrentTime
  appendFile fp $ unwords
    [ formatTime defaultTimeLocale "%c" now
    , if direction == ToServer then "--->" else "<---"
    , unpack msg
    , "\n"
    ]

-- | Events come in from the server.
logEvent :: (channel -> Text) -- ^ Pretty-print channel names.
  -> (user -> Text)           -- ^ Pretty-print user names.
  -> FilePath -> Event channel user -> IO ()
logEvent showc showu fp (Event _ mc u txt) = logRaw fp FromServer $ case mc of
  Just c  -> "[in: " <> showc c <> "] [from: " <> showu u <> "]: " <> txt
  Nothing ->                         "[from: " <> showu u <> "]: " <> txt

-- | Actions are used to instruct the backend.
logAction :: (channel -> Text) -- ^ Pretty-print channel names.
  -> (user -> Text)            -- ^ Pretty-print user names.
  -> FilePath -> Action channel user -> IO ()
logAction showc showu fp act = logRaw fp ToServer $ case act of
  Join  c -> "[join: "  <> showc c <> "]"
  Leave c -> "[leave: " <> showc c <> "]"
  Say c []  m -> "[in: " <> showc c <> "]: " <> m
  Say c us  m -> "[in: " <> showc c <> "] [to: " <> T.intercalate ", " (map showu us) <> "]: " <> m
  Whisper u m -> "[to: " <> showu u <> "]: " <> m
  Terminate -> "[stop]"
