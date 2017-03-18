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
  ) where

import Data.ByteString (ByteString)
import Data.Text (unpack)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)

import Yukibot.Types

-- | Create a logger.
logger :: Tag -> FilePath -> Logger
logger tag fp = Logger
  { toServer   = logInternal " ---> " tag fp
  , fromServer = logInternal " <--- " tag fp
  }

-- | Create a logger from a backend.
loggerFromBackend :: Backend -> Logger
loggerFromBackend b = logger (Tag $ describe b) (logFile b)

-------------------------------------------------------------------------------
-- Helpers

-- | Log a message.
logInternal :: String -> Tag -> FilePath -> ByteString -> IO ()
logInternal arrow (Tag tag) fp msg = do
  now <- getCurrentTime
  let logEntry = unwords [formatTime defaultTimeLocale "%c" now, arrow, init . tail . show $ msg]
  appendFile fp (logEntry ++ "\n")
  putStrLn $ unpack tag ++ " " ++ logEntry
