{-# LANGUAGE OverloadedStrings #-}

-- |Per-channel and per-network permission settings.
module Network.IRC.Asakura.Permissions
  ( -- *Permission levels
    PermissionLevel(..)
  -- *State
  , PermissionState
  , PermissionStateSnapshot(..)
  , defaultPermissionState
  -- *Integration
  , wrapsCmd
  , wrapsEv
  -- *Checking permissions
  , getPermission
  , hasPermission
  -- *Updating permissions
  , setNetworkPermission
  , setChannelPermission
  , delNetworkPermission
  , delChannelPermission
  ) where

import Control.Concurrent.STM (STM, atomically, readTVar, writeTVar)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)
import Data.List (sort)
import Data.Maybe (catMaybes, listToMaybe)
import Data.Text (Text)
import Network.IRC.Client (ConnectionConfig(..), Event(..), Source(..), getConnectionConfig, reply)

import Network.IRC.Asakura.Commands
import Network.IRC.Asakura.Permissions.State
import Network.IRC.Asakura.Types

-- *Integration

-- |Take as input a command, and produce a new command which will only
-- run if the user meets the required minimum permission.
wrapsCmd :: PermissionState -> PermissionLevel -> CommandDef -> CommandDef
wrapsCmd pstate perm cdef = cdef { _action = wrapped $ _action cdef } where
  wrapped f args ircstate ev = do
    allowed <- isAllowed ircstate ev

    if allowed
    then f args ircstate ev
    else berate ev

  isAllowed ircstate ev = do
    let host = _server $ getConnectionConfig ircstate

    case _source ev of
      Channel c n -> hasPermission pstate n host (Just c) perm
      User n      -> hasPermission pstate n host Nothing  perm
      _           -> return False

  berate ev = return . reply ev $ "I'm sorry, I'm afraid I can't do that."

-- |Take as input an event handler, and produce a new event handler
-- which will only run if the user meets the required minimum
-- permission.
wrapsEv :: PermissionState -> PermissionLevel -> AsakuraEventHandler -> AsakuraEventHandler
wrapsEv pstate perm edef = edef { _eventFunc = wrapped $ _eventFunc edef } where
  wrapped f ircstate ev = do
    allowed <- isAllowed ircstate ev

    if allowed
    then f ircstate ev
    else berate ev

  isAllowed ircstate ev = do
    let host = _server $ getConnectionConfig ircstate

    case _source ev of
      Channel c n -> hasPermission pstate n host (Just c) perm
      User n      -> hasPermission pstate n host Nothing  perm
      _           -> return False

  berate ev = return . reply ev $ "I'm sorry, I'm afraid I can't do that."

-- *Checking permissions

-- |Get the permission level of a user.
getPermission :: MonadIO m
  => PermissionState
  -- ^The state
  -> Text
  -- ^The nick
  -> ByteString
  -- ^The network
  -> Maybe Text
  -- ^The channel (if relevent)
  -> m (Maybe PermissionLevel)
getPermission state nick network channel = liftIO . atomically $ getPermissionByUser state nick network channel

-- |Check if a user's permission level is enough to match a
-- requirement.
hasPermission :: MonadIO m
  => PermissionState
  -- ^The state
  -> Text
  -- ^The nick
  -> ByteString
  -- ^The network
  -> Maybe Text
  -- ^The channel (if relevent)
  -> PermissionLevel
  -- ^The minimum required permission
  -> m Bool
hasPermission state nick network channel req = liftIO . atomically $ do
  perm <- getPermissionByUser state nick network channel
  return . maybe False (>=req) $ perm
  
-- |Get the permission level of a user.
getPermissionByUser :: PermissionState -> Text -> ByteString -> Maybe Text -> STM (Maybe PermissionLevel)
getPermissionByUser state nick net Nothing     = getPermissionByDef state $ PNet nick net
getPermissionByUser state nick net (Just chan) = do
  chanperm <- getPermissionByDef state $ PChan nick net chan
  netperm  <- getPermissionByDef state $ PNet  nick net

  return . listToMaybe . sort $ catMaybes [chanperm, netperm]

-- |Get the permission level of a a particular permission definition.
getPermissionByDef :: PermissionState -> PermissionDef -> STM (Maybe PermissionLevel)
getPermissionByDef state pdef = lookup pdef <$> readTVar (_permissions state)

-- *Updating permissions

-- |Set a network-level permission on a user. This will override the
-- previous network-level permission, if there was one.
setNetworkPermission :: MonadIO m
  => PermissionState 
  -- ^The state
  -> Text
  -- ^The nick
  -> ByteString
  -- ^The network
  -> PermissionLevel
  -- ^The new permission level
  -> m ()
setNetworkPermission state nick network new = liftIO . atomically $ overridePermission state (PNet nick network) new

-- |Set a channel-level permission on a user. This will override the
-- previous channel-level permission, if there was one.
setChannelPermission :: MonadIO m
  => PermissionState 
  -- ^The state
  -> Text
  -- ^The nick
  -> ByteString
  -- ^The network
  -> Text
  -- ^The channel name
  -> PermissionLevel
  -- ^The new permission level
  -> m ()
setChannelPermission state nick network channel new = liftIO . atomically $ overridePermission state (PChan nick network channel) new

-- |Remove the currently active (if any) network-level permission from
-- a user.
delNetworkPermission :: MonadIO m
  => PermissionState 
  -- ^The state
  -> Text
  -- ^The nick
  -> ByteString
  -- ^The network
  -> m ()
delNetworkPermission state nick network = liftIO . atomically $ rmPermission state (PNet nick network)

-- |Remove the currently active (if any) channel-level permission from
-- a user.
delChannelPermission :: MonadIO m
  => PermissionState 
  -- ^The state
  -> Text
  -- ^The nick
  -> ByteString
  -- ^The network
  -> Text
  -- ^The channel name
  -> m ()
delChannelPermission state nick network channel = liftIO . atomically $ rmPermission state (PChan nick network channel)

-- |Override a permission level.
overridePermission :: PermissionState -> PermissionDef -> PermissionLevel -> STM ()
overridePermission state pdef plev = rmPermission state pdef >> setPermission state pdef plev

-- |Delete a permission level.
rmPermission :: PermissionState -> PermissionDef -> STM ()
rmPermission state pdef = do
  let tvarP = _permissions state

  perms <- readTVar tvarP
  writeTVar tvarP $ filter ((/=pdef) . fst) perms

-- |Set a permission level.
setPermission :: PermissionState -> PermissionDef -> PermissionLevel -> STM ()
setPermission state pdef plev = do
  let tvarP = _permissions state

  perms <- readTVar tvarP
  writeTVar tvarP $ (pdef, plev) : perms
