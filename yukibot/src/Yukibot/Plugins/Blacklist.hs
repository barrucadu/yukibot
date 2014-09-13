{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

-- |Blacklist plugins on a per-channel basis
module Yukibot.Plugins.Blacklist 
    ( -- *State
      BlacklistState
    -- *Snapshotting
    , BlacklistStateSnapshot
      -- *Commands
    , blacklistCmd
    , whitelistCmd
    -- *Integration
    , blacklist
    , whitelist
    , ifNotBlacklisted
    , wraps
    , wrapsCmd
    ) where

import Control.Applicative       ((<$>))
import Control.Arrow             (first)
import Control.Concurrent.STM    (TVar, atomically, newTVar, readTVar, writeTVar)
import Control.Lens              ((&), (^.), (%~), at, non)
import Control.Monad             (liftM)
import Control.Monad.IO.Class    (MonadIO, liftIO)
import Data.Aeson                (FromJSON(..), ToJSON(..))
import Data.ByteString           (ByteString)
import Data.ByteString.Char8     (pack, unpack)
import Data.Default.Class        (Default(..))
import Data.Map                  (Map)
import Data.Maybe                (fromMaybe)
import Data.Text                 (Text, isPrefixOf)
import Network.IRC.Asakura.State (Snapshot(..), Rollback(..))
import Network.IRC.Asakura.Types (AsakuraEventHandler(..), Bot)
import Network.IRC.Client        (reply)
import Network.IRC.Client.Types  (ConnectionConfig(_server), Event(..), IRC, IRCState, Source(..), UnicodeEvent, connectionConfig, getConnectionConfig)

import qualified Data.Map as M

-- *State

newtype BlacklistState = BS { _blacklist :: TVar (Map ByteString (Map Text [Text])) }

-- *Snapshotting

newtype BlacklistStateSnapshot = BSS { _ssBlacklist :: Map String (Map Text [Text]) }

instance FromJSON BlacklistStateSnapshot where
    parseJSON = fmap BSS . parseJSON

instance ToJSON BlacklistStateSnapshot where
    toJSON = toJSON . _ssBlacklist

instance Snapshot BlacklistState BlacklistStateSnapshot where
    snapshotSTM bs = liftM (BSS . toStr) (readTVar $ _blacklist bs)
        where toStr = M.fromList . map (first unpack) . M.toList

instance Rollback BlacklistStateSnapshot BlacklistState where
    rollbackSTM bss = liftM BS (newTVar . fromStr $ _ssBlacklist bss)
        where fromStr = M.fromList . map (first pack) . M.toList

instance Default BlacklistStateSnapshot where
    def = BSS M.empty

-- *Commands

-- |Usage: "<command> <plugin>" in channel, or "<command> <channel>
-- <plugin>" in query
blacklistCmd :: BlacklistState -> [Text] -> IRCState -> UnicodeEvent -> Bot (IRC ())
blacklistCmd bs args _ = doCmd (blacklist bs) args

-- |Same usage as 'blacklistCmd'.
whitelistCmd :: BlacklistState -> [Text] -> IRCState -> UnicodeEvent -> Bot (IRC ())
whitelistCmd bs args _ = doCmd (whitelist bs) args

doCmd :: (ByteString -> Text -> Text -> IRC ()) -> [Text] -> UnicodeEvent -> Bot (IRC ())
doCmd f (x:xs) ev = return $ do
  network <- _server <$> connectionConfig

  case _source ev of
    Channel c _ -> mapM_ (f network c) $ x:xs
    _ | "#" `isPrefixOf` x -> mapM_ (f network x) xs
      | otherwise -> reply ev "Which channel?"

doCmd _ [] ev = return . reply ev $ "Name at least one plugin."

-- *Integration

-- |Blacklist a plugin in a channel
blacklist :: MonadIO m => BlacklistState -> ByteString -> Text -> Text -> m ()
blacklist bs network channel plugin = liftIO . atomically $ do
  let tvarB = _blacklist bs
  bl <- readTVar tvarB
  writeTVar tvarB $ alterBL bl network channel (plugin:)

-- |Whitelist a plugin in a channel
whitelist :: MonadIO m => BlacklistState -> ByteString -> Text -> Text -> m ()
whitelist bs network channel plugin = liftIO . atomically $ do
  let tvarB = _blacklist bs
  bl <- readTVar tvarB
  writeTVar tvarB $ alterBL bl network channel (filter (/=plugin))

-- |Event handler channel filter using a blacklist
ifNotBlacklisted :: MonadIO m => BlacklistState -> Text -> ByteString -> Text -> m Bool
ifNotBlacklisted bs plugin network channel = liftIO . atomically $ do
  let tvarB = _blacklist bs
  bl <- readTVar tvarB
  return . not $ plugin `elem` (bl ^. at network . non M.empty . at channel . non [])

-- |Produce a new event handler which respects the blacklist
wraps :: BlacklistState -> Text -> AsakuraEventHandler -> AsakuraEventHandler
wraps bs plugin evh = evh { _appliesTo = ifNotBlacklisted bs plugin }

-- |Produce a new command which respects the blacklist
wrapsCmd :: BlacklistState -> Text -> ([Text] -> IRCState -> UnicodeEvent -> Bot (IRC ())) -> [Text] -> IRCState -> UnicodeEvent -> Bot (IRC ())
wrapsCmd bs plugin cmd args ircstate ev = do
  let network = _server $ getConnectionConfig ircstate

  case _source ev of
    Channel c _ -> do
      ifbl <- ifNotBlacklisted bs plugin network c
      if ifbl
      then cmd args ircstate ev
      else return $ return ()

    _ -> cmd args ircstate ev

alterBL :: Map ByteString (Map Text [Text]) -> ByteString -> Text -> ([Text] -> [Text]) -> Map ByteString (Map Text [Text])
alterBL bl network channel f = bl & at network . non M.empty . at channel %~ f'
    where f' xs = case f $ fromMaybe [] xs of
                    [] -> Nothing
                    ys -> Just ys
