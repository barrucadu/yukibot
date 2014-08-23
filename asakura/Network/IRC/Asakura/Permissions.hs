-- |Per-channel and per-network permission settings.
--
-- TODO: Event handler to automatically update the permissions of
-- chanops.
module Network.IRC.Asakura.Permissions
    ( -- *Permission levels
      PermissionLevel(..)
    -- *State
    , PermissionState
    , initialise
    -- *Checking permissions
    , getPermission
    , hasPermission
    -- *Updating permissions
    , setNetworkPermission
    , setChannelPermission
    , delNetworkPermission
    , delChannelPermission
    ) where

import Control.Applicative    ((<$>))
import Control.Concurrent.STM (STM, TVar, atomically, newTVar, readTVar, writeTVar)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.List              (sort)
import Data.Maybe             (catMaybes, listToMaybe)
import Data.Ord               (Down(..), comparing)
import Data.Text              (Text)
import Network                (HostName)

-- *Permission levels

-- |Users are divided up into three classes. In the Admin/TrustedUser
-- classes, lower integers represent higher permission levels (0 being
-- the highest, just like in a priority-based algorithm).
data PermissionLevel = God
                     | Admin   Int
                     | Trusted Int
                     deriving (Eq, Show)

instance Ord PermissionLevel where
    -- All gods are equal.
    compare God God = EQ

    -- Lesser mortals are ranked in reverse integer order.
    compare (Admin   i) (Admin   j) = comparing Down i j
    compare (Trusted i) (Trusted j) = comparing Down i j

    -- And constructor order determines the rest.
    compare God       _ = GT
    compare (Admin _) _ = GT

    compare _ _ = LT

-- *State

-- |A uniquely-identified permission. When determining someone's
-- permission in a given situation, the better permission takes
-- priority: so if someone is a network admin and a trusted user in
-- the channel in question, the network admin permission wins.
data PermissionDef = PChan Text HostName Text
                   -- ^Nick, network, channel
                   | PNet  Text HostName
                   -- ^Nick, network
                   deriving (Eq, Show)

data PermissionState = PermissionState
    { _permissions :: TVar [(PermissionDef, PermissionLevel)]
    -- ^List of permissions known to the bot.
    }

-- |Initialise a fresh permission state. As in the Command module,
-- this should only be done once and the state shared.
initialise :: MonadIO m => m PermissionState
initialise = do
  tvarP <- liftIO . atomically . newTVar $ []
  return PermissionState { _permissions = tvarP }

-- *Checking permissions

-- |Get the permission level of a user.
getPermission :: MonadIO m
              => PermissionState
              -- ^The state
              -> Text
              -- ^The nick
              -> HostName
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
              -> HostName
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
getPermissionByUser :: PermissionState -> Text -> HostName -> Maybe Text -> STM (Maybe PermissionLevel)
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
                     -> HostName
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
                     -> HostName
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
                     -> HostName
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
                     -> HostName
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
