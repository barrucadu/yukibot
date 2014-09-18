{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

-- |Blacklist plugins on a per-channel basis
module Network.IRC.Asakura.Blacklist 
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

import Control.Applicative          ((<$>))
import Control.Concurrent.STM       (atomically, readTVar, writeTVar)
import Control.Monad.IO.Class       (MonadIO, liftIO)
import Data.ByteString              (ByteString)
import Data.Map                     (Map)
import Data.Maybe                   (fromMaybe)
import Data.Text                    (Text, isPrefixOf)
import Network.IRC.Asakura.Blacklist.State
import Network.IRC.Asakura.Commands (CommandDef(..))
import Network.IRC.Asakura.Types    (AsakuraEventHandler(..), Bot)
import Network.IRC.Client           (reply)
import Network.IRC.Client.Types     ( ConnectionConfig(_server)
                                    , Event(..)
                                    , IRC, IRCState, Source(..)
                                    , UnicodeEvent
                                    , connectionConfig
                                    , getConnectionConfig)

import qualified Data.Map as M

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
  return . not $ plugin `elem` chan bl

  where
    chan bl = [] `fromMaybe` M.lookup channel (netw bl)
    netw bl = M.empty `fromMaybe` M.lookup network bl

-- |Produce a new event handler which respects the blacklist
wraps :: BlacklistState -> Text -> AsakuraEventHandler -> AsakuraEventHandler
wraps bs plugin evh = evh { _appliesTo = ifNotBlacklisted bs plugin }

-- |Produce a new command which respects the blacklist
wrapsCmd :: BlacklistState -> Text -> CommandDef -> CommandDef
wrapsCmd bs name cdef = cdef { _action = wrapped $ _action cdef }
  where
    wrapped f args ircstate ev = do
      let network = _server $ getConnectionConfig ircstate

      case _source ev of
        Channel c _ -> do
          ifbl <- ifNotBlacklisted bs name network c
          if ifbl
          then f args ircstate ev
          else return $ return ()

        _ -> f args ircstate ev

alterBL :: Map ByteString (Map Text [Text]) -> ByteString -> Text -> ([Text] -> [Text]) -> Map ByteString (Map Text [Text])
alterBL bl network channel f = M.alter netf network bl
  where
    netf = Just . M.alter f' channel . fromMaybe M.empty
    f' xs =
      case f $ fromMaybe [] xs of
        [] -> Nothing
        ys -> Just ys
