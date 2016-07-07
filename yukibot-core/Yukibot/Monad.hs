-- |
-- Module      : Yukibot.Monad
-- Copyright   : (c) 2016 Michael Walker
-- License     : MIT
-- Stability   : experimental
-- Portability : portable
module Yukibot.Monad
  ( -- * Monad
    BackendM
  , runBackendM
  -- * Actions
  , joinChannel
  , leaveChannel
  , reply
  , say
  , whisper
  , disconnect
  , getCommandPrefix
  , getInstance
  , getEvent
  , isDeified
  ) where

import Control.Monad.Trans.Free (liftF, iterT)
import Data.Text (Text)

import Yukibot.Backend (sendAction)
import qualified Yukibot.Plugin.Builtin as Builtin
import Yukibot.Types

-- | Run a 'BackendM' computation.
runBackendM :: Builtin.BuiltinState
  -- ^ The state of the \"builtin\" plugin.
  -> InstantiatedBackend
  -- ^ The instantiated backend.
  -> Event
  -- ^ The current event.
  -> BackendM a
  -- ^ The effectful computation to perform.
  -> IO a
runBackendM st ib ev = iterT go where
  go (SendAction act k) = do
    sendAction (eventHandle ev) act
    k
  go (Reply msg k) = do
    case eventChannel ev of
      Just cname -> sendAction (eventHandle ev) (Say cname [eventUser ev] msg)
      Nothing    -> sendAction (eventHandle ev) (Whisper (eventUser ev) msg)
    k
  go (GetCommandPrefix mcname k) =
    k =<< runBackendM st ib ev (Builtin.getCommandPrefix st)
  go (GetInstance k) = k ib
  go (GetEvent k) = k ev
  go (IsDeified k) = do
    deities <- runBackendM st ib ev (Builtin.getDeities st)
    k (eventUser ev `elem` deities)

-------------------------------------------------------------------------------
-- Actions

-- | Join a channel.
joinChannel :: ChannelName -> BackendM ()
joinChannel cname = sendActionM (Join cname)

-- | Leave a channel.
leaveChannel :: ChannelName -> BackendM ()
leaveChannel cname = sendActionM (Leave cname)

-- | Reply to the last message.
reply :: Text -> BackendM ()
reply msg = liftF $ Reply msg ()

-- | Send a message to a channel, optionally addressed to some users.
say :: ChannelName -> [UserName] -> Text -> BackendM ()
say cname users msg = sendActionM (Say cname users msg)

-- | Send a message to a user.
whisper :: UserName -> Text -> BackendM ()
whisper user msg = sendActionM (Whisper user msg)

-- | Disconnect from the backend.
disconnect :: BackendM ()
disconnect = sendActionM Terminate

-- | Get the prefix for command verbs.
getCommandPrefix :: Maybe ChannelName -> BackendM Text
getCommandPrefix cname = liftF $ GetCommandPrefix cname id

-- | Get the instantiated backend.
getInstance :: BackendM InstantiatedBackend
getInstance = liftF $ GetInstance id

-- | Get the current event.
getEvent :: BackendM Event
getEvent = liftF $ GetEvent id

-- | Check if the current user is deified.
isDeified :: BackendM Bool
isDeified = liftF $ IsDeified id

-------------------------------------------------------------------------------
-- Utilities

sendActionM :: Action -> BackendM ()
sendActionM act = liftF $ SendAction act ()
