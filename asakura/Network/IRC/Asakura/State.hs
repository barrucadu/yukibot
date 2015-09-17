{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

-- |Typeclasses for STM-y state which can be snapshotted and restored.
module Network.IRC.Asakura.State where

import Control.Concurrent.STM (STM, atomically)
import Control.Monad.IO.Class (MonadIO, liftIO)

-- |Things which can have a snapshot of their state taken.
--
-- The `shot` type SHOULD be a value, preferably which can be
-- immediately serialised with no further processing.
--
-- Minimal complete definition: snapshotSTM.
class Snapshot active shot | active -> shot, shot -> active where
  -- |Take a snapshot of the state in STM.
  snapshotSTM :: active -> STM shot

  -- |Atomically take a snapshot of the state.
  snapshot :: MonadIO m => active -> m shot
  snapshot = liftIO . atomically . snapshotSTM

-- |Things which can be restored from a snapshot of prior state.
--
-- The `active` type MAY be a function, taking other active state as
-- parameters.
--
-- Minimal complete definition: rollbackSTM.
class Rollback shot active | shot -> active, active -> shot where
  -- |Roll back a snapshot into live state.
  rollbackSTM :: shot -> STM active

  -- |Atomically roll back a snapshot into live state.
  rollback :: MonadIO m => shot -> m active
  rollback = liftIO . atomically . rollbackSTM
