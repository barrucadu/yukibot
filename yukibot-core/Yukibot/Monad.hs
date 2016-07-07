-- |
-- Module      : Yukibot.Monad
-- Copyright   : (c) 2016 Michael Walker
-- License     : MIT
-- Stability   : experimental
-- Portability : portable
module Yukibot.Monad where

import Control.Applicative ((<*))
import Control.Monad.Trans.Free (liftF, iterT)
import Data.Text (Text)

import qualified Yukibot.Backend as Backend
import qualified Yukibot.Plugin.Builtin as Builtin
import Yukibot.Types

-------------------------------------------------------------------------------
-- * Monad

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
  go (Reply msg k) = k <* Backend.sendAction (eventHandle ev) (case eventChannel ev of
    Just cname -> Say cname [eventUser ev] msg
    Nothing    -> Whisper (eventUser ev) msg)
  go (QuickReply msg k) = k <* Backend.sendAction (eventHandle ev) (case eventChannel ev of
    Just cname -> Say cname [] msg
    Nothing    -> Whisper (eventUser ev) msg)
  go (GetCommandPrefixIn   mcname k) = k =<< runBackendM st ib ev (Builtin.getCommandPrefixIn   st mcname)
  go (GetDisabledPluginsIn mcname k) = k =<< runBackendM st ib ev (Builtin.getDisabledPluginsIn st mcname)
  go (GetEnabledCommandsIn mcname k) = k =<< runBackendM st ib ev (Builtin.getEnabledCommandsIn st mcname)
  go (GetEnabledMonitorsIn mcname k) = k =<< runBackendM st ib ev (Builtin.getEnabledMonitorsIn st mcname)
  go (SendAction act k) = k <* Backend.sendAction (eventHandle ev) act
  go (GetInstance k) = k ib
  go (GetEvent    k) = k ev
  go (GetDeities  k) = k =<< runBackendM st ib ev (Builtin.getDeities st)

-------------------------------------------------------------------------------
-- * Actions

-- | Join a channel.
joinChannel :: ChannelName -> BackendM ()
joinChannel cname = sendAction (Join cname)

-- | Leave a channel.
leaveChannel :: ChannelName -> BackendM ()
leaveChannel cname = sendAction (Leave cname)

-- | Reply to the last message.
reply :: Text -> BackendM ()
reply msg = liftF $ Reply msg ()

-- | Reply to the last message, without addressing the user.
quickReply :: Text -> BackendM ()
quickReply msg = liftF $ QuickReply msg ()

-- | Send a message to a channel, optionally addressed to some users.
say :: ChannelName -> [UserName] -> Text -> BackendM ()
say cname users msg = sendAction (Say cname users msg)

-- | Send a message to a user.
whisper :: UserName -> Text -> BackendM ()
whisper user msg = sendAction (Whisper user msg)

-- | Disconnect from the backend.
disconnect :: BackendM ()
disconnect = sendAction Terminate

-- | Get the command prefix in the current channel.
getCommandPrefix :: BackendM Text
getCommandPrefix = getCommandPrefixIn . eventChannel =<< getEvent

-- | Get the command prefix in an arbitrary channel.
getCommandPrefixIn :: Maybe ChannelName -> BackendM Text
getCommandPrefixIn mcname = liftF $ GetCommandPrefixIn mcname id

-- | Get the disabled plugins in the current channel.
getDisabledPlugins :: BackendM [PluginName]
getDisabledPlugins = getDisabledPluginsIn . eventChannel =<< getEvent

-- | Get the disabled plugins in an arbitrary channel.
getDisabledPluginsIn :: Maybe ChannelName -> BackendM [PluginName]
getDisabledPluginsIn mcname = liftF $ GetDisabledPluginsIn mcname id

-- | Get the enabled commands in the current channel.
getEnabledCommands :: BackendM [(PluginName, CommandName, Text)]
getEnabledCommands = getEnabledCommandsIn . eventChannel =<< getEvent

-- | Get the enabled commands in an arbitrary channel.
getEnabledCommandsIn :: Maybe ChannelName -> BackendM [(PluginName, CommandName, Text)]
getEnabledCommandsIn mcname = liftF $ GetEnabledCommandsIn mcname id

-- | Get the enabled monitors in the current channel.
getEnabledMonitors :: BackendM [(PluginName, MonitorName)]
getEnabledMonitors = getEnabledMonitorsIn . eventChannel =<< getEvent

-- | Get the enabled monitors in an arbitrary channel.
getEnabledMonitorsIn :: Maybe ChannelName -> BackendM [(PluginName, MonitorName)]
getEnabledMonitorsIn mcname = liftF $ GetEnabledMonitorsIn mcname id

-- | Get the instantiated backend.
getInstance :: BackendM InstantiatedBackend
getInstance = liftF $ GetInstance id

-- | Get the current event.
getEvent :: BackendM Event
getEvent = liftF $ GetEvent id

-- | Get the deities.
getDeities :: BackendM [UserName]
getDeities = liftF $ GetDeities id

-- | Check if the current user is deified.
isDeified :: BackendM Bool
isDeified = do
  deities <- getDeities
  ev      <- getEvent
  pure (eventUser ev `elem` deities)

-- | Send a backend action.
sendAction :: Action -> BackendM ()
sendAction act = liftF $ SendAction act ()
