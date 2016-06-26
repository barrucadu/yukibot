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
  , loggerFromBackend
  , RawLogger(..)
  , rawLogger
  , rawLoggerFromBackend

  -- * Component loggers
  , logRawTo
  , logRawFrom
  , logEvent
  , logAction
  ) where

import Data.ByteString (ByteString)
import Data.Monoid ((<>))
import Data.Text (Text, unpack)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)

import Yukibot.Types

-- | Create an event/action logger.
logger :: (channel -> Text)
  -- ^ Pretty-print channel names.
  -> (user -> Text)
  -- ^ Pretty-print user names.
  -> Tag -> FilePath -> Logger channel user
logger showc showu tag fp = Logger
  { loggerEvent  = logEvent showc showu tag fp
  , loggerAction = logAction showc showu tag fp
  }

-- | Create an event/action logger from a backend.
loggerFromBackend :: Backend' channel user -> Logger channel user
loggerFromBackend b = logger (showChannel  b)
                             (showUser     b)
                             (Tag $ describe b)
                             (unrawLogFile b)

-- | Create a raw logger.
rawLogger :: Tag -> FilePath -> RawLogger
rawLogger tag fp = RawLogger
  { rawToServer = logRawTo tag fp
  , rawFromServer = logRawFrom tag fp
  }

-- | Create a raw logger from a backend.
rawLoggerFromBackend :: Backend' channel user -> RawLogger
rawLoggerFromBackend b = rawLogger (Tag $ describe b) (rawLogFile b)

-------------------------------------------------------------------------------
-- Component loggers

-- | A message sent from the backend to the server.
logRawTo :: Tag -> FilePath -> ByteString -> IO ()
logRawTo tag fp = logInternalTo tag fp . init . tail . show

-- | A message sent from the server to the backend.
logRawFrom :: Tag -> FilePath -> ByteString -> IO ()
logRawFrom tag fp = logInternalFrom tag fp . init . tail . show

-- | Events come in from the server.
logEvent :: (channel -> Text)
  -- ^ Pretty-print channel names.
  -> (user -> Text)
  -- ^ Pretty-print user names.
  -> Tag -> FilePath -> Event channel user -> IO ()
logEvent showc showu tag fp (Event _ mc u txt) = logInternalFrom tag fp . unpack $ case mc of
  Just c  -> "[in: " <> showc c <> "] [from: " <> showu u <> "]: " <> txt
  Nothing ->                         "[from: " <> showu u <> "]: " <> txt

-- | Actions are used to instruct the backend.
logAction :: (channel -> Text)
  -- ^ Pretty-print channel names.
  -> (user -> Text)
  -- ^ Pretty-print user names.
  -> Tag -> FilePath -> Action channel user -> IO ()
logAction showc showu tag fp act = logInternalTo tag fp . unpack $ case act of
  Join  c -> "[join: "  <> showc c <> "]"
  Leave c -> "[leave: " <> showc c <> "]"
  Say c []  m -> "[in: " <> showc c <> "]: " <> m
  Say c us  m -> "[in: " <> showc c <> "] [to: " <> T.intercalate ", " (map showu us) <> "]: " <> m
  Whisper u m -> "[to: " <> showu u <> "]: " <> m
  Terminate -> "[stop]"

-------------------------------------------------------------------------------
-- Helpers

-- | Log a message sent TO something.
logInternalTo :: Tag -> FilePath -> String -> IO ()
logInternalTo = logInternal " ---> "

-- | Log a message received FROM something.
logInternalFrom :: Tag -> FilePath -> String -> IO ()
logInternalFrom = logInternal " <--- "

-- | Log a message.
logInternal :: String -> Tag -> FilePath -> String -> IO ()
logInternal arrow (Tag tag) fp msg = do
  now <- getCurrentTime
  let logEntry = unwords [formatTime defaultTimeLocale "%c" now, arrow, msg]
  appendFile fp (logEntry ++ "\n")
  putStrLn (unpack tag ++ " " ++ logEntry)
