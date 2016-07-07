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

import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.Monoid ((<>))
import Data.Text (unpack)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)

import Yukibot.Types

-- | Create an event/action logger.
logger :: Tag -> FilePath -> Logger
logger tag fp = Logger
  { loggerEvent  = logEvent tag fp
  , loggerAction = logAction tag fp
  }

-- | Create an event/action logger from a backend.
loggerFromBackend :: Backend -> Logger
loggerFromBackend b = logger (Tag $ describe b) (unrawLogFile b)

-- | Create a raw logger.
rawLogger :: Tag -> FilePath -> RawLogger
rawLogger tag fp = RawLogger
  { rawToServer = logRawTo tag fp
  , rawFromServer = logRawFrom tag fp
  }

-- | Create a raw logger from a backend.
rawLoggerFromBackend :: Backend -> RawLogger
rawLoggerFromBackend b = rawLogger (Tag $ describe b) (rawLogFile b)

-------------------------------------------------------------------------------
-- Component loggers

-- | A message sent from the backend to the server.
logRawTo :: Tag -> FilePath -> ByteString -> IO ()
logRawTo tag fp = logInternalTo True tag fp . init . tail . show

-- | A message sent from the server to the backend.
logRawFrom :: Tag -> FilePath -> ByteString -> IO ()
logRawFrom tag fp = logInternalFrom True tag fp . init . tail . show

-- | Events come in from the server.
logEvent :: Tag -> FilePath -> Event -> IO ()
logEvent tag fp (Event _ _ mc u txt) = logInternalFrom False tag fp . unpack $ case mc of
  Just c  -> "[in: " <> getChannelName c <> "] [from: " <> getUserName u <> "]: " <> txt
  Nothing -> "[from: " <> getUserName u <> "]: " <> txt

-- | Actions are used to instruct the backend.
logAction :: Tag -> FilePath -> Action -> IO ()
logAction tag fp act = logInternalTo False tag fp . unpack $ case act of
  Join  c -> "[join: "  <> getChannelName c <> "]"
  Leave c -> "[leave: " <> getChannelName c <> "]"
  Say c []  m -> "[in: " <> getChannelName c <> "]: " <> m
  Say c us  m -> "[in: " <> getChannelName c <> "] [to: " <> T.intercalate ", " (map getUserName us) <> "]: " <> m
  Whisper u m -> "[to: " <> getUserName u <> "]: " <> m
  Terminate -> "[stop]"

-------------------------------------------------------------------------------
-- Helpers

-- | Log a message sent TO something.
logInternalTo :: Bool -> Tag -> FilePath -> String -> IO ()
logInternalTo = logInternal " ---> "

-- | Log a message received FROM something.
logInternalFrom :: Bool -> Tag -> FilePath -> String -> IO ()
logInternalFrom = logInternal " <--- "

-- | Log a message.
logInternal :: String -> Bool -> Tag -> FilePath -> String -> IO ()
logInternal arrow stdout (Tag tag) fp msg = do
  now <- getCurrentTime
  let logEntry = unwords [formatTime defaultTimeLocale "%c" now, arrow, msg]
  appendFile fp (logEntry ++ "\n")
  when stdout . putStrLn $ unpack tag ++ " " ++ logEntry
