{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Yukibot.Plugin.Builtin
-- Copyright   : (c) 2016 Michael Walker
-- License     : MIT
-- Stability   : experimental
-- Portability : OverloadedStrings, TemplateHaskell
--
-- Special functionality that regular plugins cannot provide. This is
-- mostly a normal plugin, but provides some extra functions used in
-- Yukibot.Main.
--
-- There are no monitors. The following commands are provided:
--
--     * "set-default-prefix", set the default command prefix for this
--       backend.
--
--     * "set-channel-prefix", set the custom command prefix for this
--       channel.
--
--     * "unset-channel-prefix", remove the custom command prefix for
--       this channel.
--
--     * "enable-plugin", enable a previously-disabled plugin in this
--       channel. Monitors and commands retain their prior
--       enabled/disabled state.
--
--     * "disable-plugin", disable a plugin in this channel.
--
--     * "start-monitor", start a monitor in this channel.
--
--     * "stop-monitor", stop a monitor in this channel.
--
--     * "bind", bind a new command in this channel, first argument is
--       the command name.
--
--     * "unbind", unbind a command in this channel, args are the
--       verb.
--
--     * "unbind-all", unbind all verbs for a command in this channel.
--
--     * "deify", grant a user administrator privileges for this
--       backend.
--
--     * "degrade", remove a user's administrator privileges for this
--       backend.
module Yukibot.Plugin.Builtin
  ( -- * State
    BuiltinState
  , initialState

  -- * Plugin
  , plugin

  -- * Queries
  , getCommandPrefix
  , getEnabledCommands
  , getEnabledMonitors
  , getDeities

  -- * Misc
  , notDeityMessage
  ) where

import Control.Arrow (second)
import Control.Concurrent.STM (STM, TVar, atomically, newTVar, modifyTVar, readTVar)
import Control.Lens ((&), (.~), (%~), at)
import Control.Lens.TH (makeLenses)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Free (liftF)
import Data.Foldable (toList)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H
import Data.List (nub)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import System.Random (randomIO)

import Yukibot.Configuration
import Yukibot.Types

-------------------------------------------------------------------------------
-- State

type BackendMap v = HashMap (BackendName, Text, Int) v

-- | Abstract mutable state.
newtype BuiltinState = BuiltinState (TVar (BackendMap BackendState))

-- | State about a backend.
data BackendState = BackendState
  { _defaultPrefix :: Text
  -- ^ Command prefix.
  , _channelPrefixes :: HashMap ChannelName Text
  -- ^ Per-channel command prefix.
  , _disabledPlugins :: HashMap (Maybe ChannelName) [PluginName]
  -- ^ Plugins default to enabled.
  , _enabledMonitors :: HashMap (Maybe ChannelName) [(PluginName, MonitorName)]
  -- ^ Monitors default to disabled.
  , _defaultMonitors :: [(PluginName, MonitorName)]
  -- ^ Default monitors to enable when joining a new channel.
  , _enabledCommands :: HashMap (Maybe ChannelName) [(PluginName, CommandName, Text)]
  -- ^ Commands default to disabled.
  , _defaultCommands :: [(PluginName, CommandName, Text)]
  -- ^ Commands to enable when joining a new channel.
  , _deifiedUsers    :: [UserName]
  -- ^ Users who can execute commands from this plugin.
  }

makeLenses ''BackendState

-- | Create the initial state.
initialState :: Table -> IO BuiltinState
initialState cfg0 = atomically $ do
  var <- newTVar states
  pure (BuiltinState var)

  where
    defPrefixes = initialDefaultPrefixes cfg0
    chanPrefixes = initialChannelPrefixes cfg0
    defMonitors = initialDefaultMonitors cfg0
    defCommands = initialDefaultCommands cfg0
    deities = initialDeities cfg0
    backends = nub $ H.keys defPrefixes ++
                     H.keys chanPrefixes ++
                     H.keys defMonitors ++
                     H.keys defCommands ++
                     H.keys deities
    states = H.fromList [ (b, state b) | b <- backends ]
    state b = BackendState { _defaultPrefix   = H.lookupDefault "!" b defPrefixes
                           , _channelPrefixes = H.lookupDefault H.empty b chanPrefixes
                           , _disabledPlugins = H.empty
                           , _enabledMonitors = H.empty
                           , _defaultMonitors = H.lookupDefault [] b defMonitors
                           , _enabledCommands = H.empty
                           , _defaultCommands = H.lookupDefault [] b defCommands
                           , _deifiedUsers    = H.lookupDefault [] b deities
                           }

-- | Get the default prefixes.
initialDefaultPrefixes :: Table -> BackendMap Text
initialDefaultPrefixes = buildBackendMap (getString "default-prefix")

-- | Get the channel prefixes.
initialChannelPrefixes :: Table -> BackendMap (HashMap ChannelName Text)
initialChannelPrefixes = buildBackendMap $ \cfg -> Just . H.fromList $
  [ (ChannelName cname, prefix)
  | let tbl = getTable "channel-prefixes" cfg
  , (cname, VString prefix) <- maybe [] H.toList tbl
  ]

-- | Get the default monitors.
initialDefaultMonitors :: Table -> BackendMap [(PluginName, MonitorName)]
initialDefaultMonitors = buildBackendMap $ \cfg -> Just
  [ (PluginName pname, MonitorName mname)
  | monitor <- getStrings "monitors" cfg
  , let (pname, mname) = second T.tail (T.breakOn ":" monitor)
  , not (T.null mname)
  ]

-- | Get the default commands.
initialDefaultCommands :: Table -> BackendMap [(PluginName, CommandName, Text)]
initialDefaultCommands = buildBackendMap $ \cfg -> Just
  [ (PluginName pname, CommandName cname, verb)
  | (verb, VString cmd) <- maybe [] H.toList $ getTable "commands" cfg
  , let (pname, cname) = second T.tail (T.breakOn ":" cmd)
  , not (T.null cname)
  , not (null $ T.words verb)
  ]

-- | Get the deities.
initialDeities :: Table -> BackendMap [UserName]
initialDeities = buildBackendMap (Just . map UserName . getStrings "deities")

-------------------------------------------------------------------------------
-- Plugin

data OnOff = On | Off deriving (Eq, Ord, Read, Show, Enum, Bounded)

plugin :: BuiltinState -> config -> Either error Plugin
plugin state _ = Right Plugin
  { pluginMonitors = H.empty
  , pluginCommands = H.fromList $ map (\(cn, cf) -> (cn, wrapCommand cf state))
    [ ("set-default-prefix",   setDefaultPrefix)
    , ("set-channel-prefix",   setChannelPrefix)
    , ("unset-channel-prefix", unsetChannelPrefix)
    , ("enable-plugin",  onOffPlugin  On)
    , ("disable-plugin", onOffPlugin  Off)
    , ("start-monitor",  onOffMonitor On)
    , ("stop-monitor",   onOffMonitor Off)
    , ("bind",       bindCommand)
    , ("unbind",     unbindCommand)
    , ("unbind-all", unbindAllCommand)
    , ("deify",      deify)
    , ("degrade",    degrade)
    ]
  }

-- | Set the default prefix for a backend.
setDefaultPrefix :: BuiltinState -> Command
setDefaultPrefix st = Command $ \_ args ->
  modifyState st $ \state -> state & defaultPrefix .~ T.unwords args

-- | Set the custom prefix for a channel. If applied outside of a
-- channel, this command does nothing.
setChannelPrefix :: BuiltinState -> Command
setChannelPrefix st = Command $ \ev args ->
  modifyState st $ \state -> case eventChannel ev of
    Just c  -> state & channelPrefixes %~ H.insert c (T.unwords args)
    Nothing -> state

-- | Unset the custom prefix for a channel. If applied outside of a
-- channel, this command does nothing.
unsetChannelPrefix :: BuiltinState -> Command
unsetChannelPrefix st = Command $ \ev _ ->
  modifyState st $ \state -> case eventChannel ev of
    Just c  -> state & channelPrefixes %~ H.delete c
    Nothing -> state

-- | Enable or disable a plugin for a channel. If not in a channel,
-- this applies to whispers.
onOffPlugin :: OnOff -> BuiltinState -> Command
onOffPlugin mode st = Command $ \ev args ->
  let plugins = map PluginName args
      go = if mode == On then filter (`notElem` plugins) else nub . (plugins++)
  in modifyState st $ disabledPlugins . at (eventChannel ev) %~ (Just . go . fromMaybe [])

-- | Enable or disable a monitor for a channel. If not in a channel,
-- this applies to whispers.
onOffMonitor :: OnOff -> BuiltinState -> Command
onOffMonitor mode st = Command $ \ev args ->
  let monitors = [ (PluginName pn, MonitorName mn)
                 | arg <- args
                 , let (pn, mn) = second T.tail (T.breakOn ":" arg)
                 , not (T.null mn)
                 ]
      go = if mode == On then nub . (monitors++) else filter (`notElem` monitors)
  in modifyState st $ enabledMonitors . at (eventChannel ev) %~ (Just . go . fromMaybe [])

-- | Bind a command.
bindCommand :: BuiltinState -> Command
bindCommand st = Command $ \ev args -> case args of
  (cmd:vs) ->
    let (pname, cname) = second T.tail (T.breakOn ":" cmd)
        go = ((PluginName pname, CommandName cname, T.unwords vs):)
    in if T.null cname || null vs
       then pure ()
       else modifyState st $ enabledCommands . at (eventChannel ev) %~ (Just . go . fromMaybe [])
  _ -> pure ()

-- | Unbind a command.
unbindCommand :: BuiltinState -> Command
unbindCommand st = Command $ \ev args ->
  let go = filter (\(_,_,vs) -> vs /= T.unwords args)
  in if null args
     then pure ()
     else modifyState st $ enabledCommands . at (eventChannel ev) %~ (Just . go . fromMaybe [])

-- | Unbind all commands of the given type.
unbindAllCommand :: BuiltinState -> Command
unbindAllCommand st = Command $ \ev args ->
  let commands = [ (PluginName pn, CommandName cn)
                 | arg <- args
                 , let (pn, cn) = second T.tail (T.breakOn ":" arg)
                 , not (T.null cn)
                 ]
      go = filter (\(cn,pn,_) -> (cn,pn) `notElem` commands)
  in modifyState st $ enabledCommands . at (eventChannel ev) %~ (Just . go . fromMaybe [])

-- | Deify a list of users.
deify :: BuiltinState -> Command
deify st = Command $ \_ args ->
  let users = map UserName args
  in modifyState st $ deifiedUsers %~ (nub . (users++))

-- | Un-deify a list of users.
degrade :: BuiltinState -> Command
degrade st = Command $ \_ args ->
  let users = map UserName args
  in modifyState st $ deifiedUsers %~ filter (`notElem` users)

-------------------------------------------------------------------------------
-- Queries

-- | Get the command prefix.
getCommandPrefix :: BuiltinState
  -> InstantiatedBackend
  -> Maybe ChannelName
  -> IO Text
getCommandPrefix (BuiltinState statesVar) ib cname = atomically $ do
  state <- bmLookup ib <$> readTVar statesVar
  let def = _defaultPrefix state
  pure $ case cname of
    Just cname' -> H.lookupDefault def cname' (_channelPrefixes state)
    Nothing -> def

-- | Get the enabled commands. If there is no enabled command map for
-- this channel, copy over the defaults.
getEnabledCommands :: BuiltinState
  -> InstantiatedBackend
  -> Maybe ChannelName
  -> IO [(PluginName, CommandName, Text)]
getEnabledCommands st@(BuiltinState statesVar) ib cname = atomically $ do
  state <- bmLookup ib <$> readTVar statesVar
  let def = _defaultCommands state
  case H.lookup cname (_enabledCommands state) of
    Just enabled -> pure enabled
    Nothing -> do
      setupNewChannel st ib cname
      pure def

-- | Get the enabled monitors.
getEnabledMonitors :: BuiltinState
  -> InstantiatedBackend
  -> Maybe ChannelName
  -> IO [(PluginName, MonitorName)]
getEnabledMonitors st@(BuiltinState statesVar) ib cname = atomically $ do
  state <- bmLookup ib <$> readTVar statesVar
  let def = _defaultMonitors state
  case H.lookup cname (_enabledMonitors state) of
    Just enabled -> pure enabled
    Nothing -> do
      setupNewChannel st ib cname
      pure def

-- | Get the deities.
getDeities :: BuiltinState
  -> InstantiatedBackend
  -> IO [UserName]
getDeities (BuiltinState statesVar) ib = atomically $ do
  state <- bmLookup ib <$> readTVar statesVar
  pure (_deifiedUsers state)

-------------------------------------------------------------------------------
-- 'BackendMap's

-- | Modify the state of the current backend.
modifyState :: BuiltinState -> (BackendState -> BackendState) -> BackendM ()
modifyState (BuiltinState statesVar) f = do
  ib <- liftF (GetInstance id)
  liftIO . atomically $ do
    state <- bmLookup ib <$> readTVar statesVar
    modifyTVar statesVar (bmInsert ib $ f state)

-- | Set up the state for a new channel.
setupNewChannel :: BuiltinState
  -> InstantiatedBackend
  -> Maybe ChannelName
  -> STM ()
setupNewChannel (BuiltinState statesVar) ib cname = do
  state <- bmLookup ib <$> readTVar statesVar
  let state'  = state  & enabledMonitors . at cname .~ Just (_defaultMonitors state)
  let state'' = state' & enabledCommands . at cname .~ Just (_defaultCommands state)
  modifyTVar statesVar (bmInsert ib state'')

-- | Look up a value in a 'BackendMap'.
bmLookup :: InstantiatedBackend -> BackendMap v -> v
bmLookup ib = H.lookupDefault (error "Missing backend state!") sig where
  sig = (instBackendName ib, instSpecificName ib, instIndex ib)

-- | Insert a value into a 'BackendMap'.
bmInsert :: InstantiatedBackend -> v -> BackendMap v -> BackendMap v
bmInsert ib = H.insert sig where
  sig = (instBackendName ib, instSpecificName ib, instIndex ib)

-- | Build a 'BackendMap' by extracting every instance from the
-- configuration.
buildBackendMap :: (Table -> Maybe v) -> Table -> BackendMap v
buildBackendMap f cfg0 = H.fromList
  [ (sig, v)
  | (bname, VTable cfgs) <- maybe [] H.toList (getTable "backend" cfg0)
  , (sname, cfg)         <- H.toList cfgs
  , (index, Just v)      <- go cfg
  , let sig = (BackendName bname, sname, index)
  ]

  where
    go (VTable  c)  = [(0, f (c `override` cfg0))]
    go (VTArray cs) = [(i, f (c `override` cfg0)) | (i, c) <- zip [0..] (toList cs)]
    go _ = [(0, f cfg0)]

-------------------------------------------------------------------------------
-- Utilities

-- | Check that the user of a command is a deity.
wrapCommand :: (BuiltinState -> Command) -> BuiltinState -> Command
wrapCommand cf st = Command $ \ev args -> do
  let (Command cmd) = cf st
  isDeified <- liftF (IsDeified id)
  if isDeified
    then cmd ev args
    else notDeityMessage >>= \msg -> liftF (Reply msg ())

-- | A random message to be given when the user is not a deity.
notDeityMessage :: MonadIO m => m Text
notDeityMessage = liftIO $ do
  let messages = [ "You are not an administrator."
                 , "I'm afraid I can't let you do that."
                 , "Such power is not meant for mere mortals."
                 , "Only the chosen few have that power!"
                 , "You are trying to mess with forces beyond your comprehension!"
                 , "No."
                 , "LA LA LA I'M NOT LISTENING TO YOU."
                 , "I feel like you *thought* your words had meaning, but they didn't."
                 ]
  idx <- randomIO
  pure $ messages !! (idx `mod` length messages)
