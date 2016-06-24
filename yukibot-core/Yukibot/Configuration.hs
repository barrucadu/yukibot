-- |
-- Module      : Yukibot.Configuration
-- Copyright   : (2) 2016 Michael Walker
-- License     : MIT
-- Stability   : experimental
-- Portability : portable
module Yukibot.Configuration
  ( -- * Parsing
    parseConfigFile
    -- * Accessors
    , getArray
    , getBool
    , getDouble
    , getInteger
    , getString
    , getTable
    , getTableArray
    , getUTCTime
    -- * Re-exports
  , module Text.Toml.Types
  ) where

import Data.Foldable (toList)
import qualified Data.HashMap.Strict as H
import Data.Text (Text, pack)
import Data.Time.Clock (UTCTime)
import System.FilePath (FilePath)
import Text.Toml (parseTomlDoc)
import Text.Toml.Types

-- | Parse a configuration file
parseConfigFile :: FilePath -> IO (Maybe Table)
parseConfigFile path = do
  toml <- parseTomlDoc "" . pack <$> readFile path
  pure $ either (const Nothing) Just toml

-------------------------------------------------------------------------------
-- Accessors

getArray :: Text -> Table -> Maybe [Node]
getArray fld tbl = case H.lookup fld tbl of
  Just (VArray a) -> Just (toList a)
  _ -> Nothing

getBool :: Text -> Table -> Maybe Bool
getBool fld tbl = case H.lookup fld tbl of
  Just (VBoolean b) -> Just b
  _ -> Nothing

getDouble :: Text -> Table -> Maybe Double
getDouble fld tbl = case H.lookup fld tbl of
  Just (VFloat f) -> Just f
  _ -> Nothing

getInteger :: Integral i => Text -> Table -> Maybe i
getInteger fld tbl = case H.lookup fld tbl of
  Just (VInteger i) -> Just (fromIntegral i)
  _ -> Nothing

getString :: Text -> Table -> Maybe Text
getString fld tbl = case H.lookup fld tbl of
  Just (VString t) -> Just t
  _ -> Nothing

getTable :: Text -> Table -> Maybe Table
getTable fld tbl = case H.lookup fld tbl of
  Just (VTable b) -> Just b
  _ -> Nothing

getTableArray :: Text -> Table -> Maybe [Table]
getTableArray fld tbl = case H.lookup fld tbl of
  Just (VTArray ts) -> Just (toList ts)
  _ -> Nothing

getUTCTime :: Text -> Table -> Maybe UTCTime
getUTCTime fld tbl = case H.lookup fld tbl of
  Just (VDatetime b) -> Just b
  _ -> Nothing
