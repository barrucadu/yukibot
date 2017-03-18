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
import System.IO (BufferMode(..), Handle, IOMode(..), hClose, hFlush, hPutStrLn, hSetBuffering, openFile)

import Yukibot.Types

-- | Create a logger.
logger :: Tag -> Handle -> Logger
logger tag fh = Logger
  { toServer   = logInternal " ---> " tag fh
  , fromServer = logInternal " <--- " tag fh
  , flushLog   = hFlush fh
  , closeLog   = hClose fh
  }

-- | Create a logger from a backend, opening the log file.
loggerFromBackend :: Backend -> IO Logger
loggerFromBackend b = do
  fh <- openFile (logFile b) AppendMode
  hSetBuffering fh LineBuffering
  pure (logger (Tag $ describe b) fh)

-------------------------------------------------------------------------------
-- Helpers

-- | Log a message.
logInternal :: String -> Tag -> Handle -> ByteString -> IO ()
logInternal arrow (Tag tag) fh msg = do
  now <- getCurrentTime
  let logEntry = unwords [formatTime defaultTimeLocale "%c" now, arrow, init . tail . show $ msg]
  hPutStrLn fh logEntry
  putStrLn $ unpack tag ++ " " ++ logEntry
