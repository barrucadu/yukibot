{-# LANGUAGE MultiParamTypeClasses #-}

module Network.IRC.Bot.Blacklist.State where

import Control.Arrow (first)
import Control.Concurrent.STM (TVar, newTVar, readTVar)
import Control.Monad (liftM)
import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack, unpack)
import Data.Map (Map)
import Data.Text (Text)

import Network.IRC.Bot.State

import qualified Data.Map as M

-- *State

newtype BlacklistState = BS { _blacklist :: TVar (Map ByteString (Map Text [Text])) }

-- *Snapshotting

newtype BlacklistStateSnapshot = BSS { _ssBlacklist :: Map String (Map Text [Text]) }

-- | Nothing blacklisted.
defaultBlacklistState :: BlacklistStateSnapshot
defaultBlacklistState = BSS M.empty

instance FromJSON BlacklistStateSnapshot where
  parseJSON = fmap BSS . parseJSON

instance ToJSON BlacklistStateSnapshot where
  toJSON = toJSON . _ssBlacklist

instance Snapshot BlacklistState BlacklistStateSnapshot where
  snapshotSTM bs = liftM (BSS . toStr) (readTVar $ _blacklist bs) where
    toStr = M.fromList . map (first unpack) . M.toList

instance Rollback BlacklistStateSnapshot BlacklistState where
  rollbackSTM bss = liftM BS (newTVar . fromStr $ _ssBlacklist bss) where
    fromStr = M.fromList . map (first pack) . M.toList
