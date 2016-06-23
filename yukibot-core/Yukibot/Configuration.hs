-- |
-- Module      : Yukibot.Configuration
-- Copyright   : (2) 2016 Michael Walker
-- License     : MIT
-- Stability   : experimental
-- Portability : portable
module Yukibot.Configuration
  ( -- * Parsing
    parseConfigFile
    -- * Re-exports
  , module Text.Toml.Types
  ) where

import Data.Text (pack)
import System.FilePath (FilePath)
import Text.Toml (parseTomlDoc)
import Text.Toml.Types

-- | Parse a configuration file
parseConfigFile :: FilePath -> IO (Maybe Table)
parseConfigFile path = do
  toml <- parseTomlDoc "" . pack <$> readFile path
  pure $ either (const Nothing) Just toml
