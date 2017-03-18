{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Module      : Yukibot.Plugin.Builtin
-- Copyright   : (c) 2016 Michael Walker
-- License     : MIT
-- Stability   : experimental
-- Portability : OverloadedStrings, RankNTypes
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
--
--     * "help", display help about a plugin, monitor, or command.
--       This does not require the user to be a deity.
module Yukibot.Plugin.Builtin
  ( -- * State
    BuiltinState
  , initialState

  -- * Plugin
  , plugin

  -- * Queries
  , getCommandPrefix
  , getCommandPrefixIn
  , getDisabledPlugins
  , getDisabledPluginsIn
  , getEnabledCommands
  , getEnabledCommandsIn
  , getEnabledMonitors
  , getEnabledMonitorsIn
  , getDeities
  , isPlugin
  , isMonitor
  , isCommand

  -- * Misc
  , notDeityMessage
  ) where

import Control.Arrow (second)
import Control.Concurrent.STM (STM, TVar, atomically, newTVar, modifyTVar, readTVar)
import Control.Lens (Lens', (&), (.~), (%~), at, lens, view)
import Control.Monad (filterM, unless)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Free (liftF)
import Data.Foldable (toList)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H
import Data.List ((\\), nub, sort)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import System.Random (randomIO)

import Yukibot.Configuration
import Yukibot.Types

-------------------------------------------------------------------------------
-- State

type BackendMap v = HashMap BackendSig v

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

defaultPrefix :: Lens' BackendState Text
defaultPrefix = lens _defaultPrefix $ \a b -> a { _defaultPrefix = b }

channelPrefixes :: Lens' BackendState (HashMap ChannelName Text)
channelPrefixes = lens _channelPrefixes $ \a b -> a { _channelPrefixes = b }

disabledPlugins :: Lens' BackendState (HashMap (Maybe ChannelName) [PluginName])
disabledPlugins = lens _disabledPlugins $ \a b -> a { _disabledPlugins = b }

enabledMonitors :: Lens' BackendState (HashMap (Maybe ChannelName) [(PluginName, MonitorName)])
enabledMonitors = lens _enabledMonitors $ \a b -> a { _enabledMonitors = b }

enabledCommands :: Lens' BackendState (HashMap (Maybe ChannelName) [(PluginName, CommandName, Text)])
enabledCommands = lens _enabledCommands $ \a b -> a { _enabledCommands = b }

deifiedUsers :: Lens' BackendState [UserName]
deifiedUsers = lens _deifiedUsers $ \a b -> a { _deifiedUsers = b }

-- | Create the initial state.
initialState :: Table -> IO BuiltinState
initialState cfg0 = atomically $ do
  var <- newTVar states
  pure (BuiltinState var)

  where
    defPrefixes = initialDefaultPrefixes cfg0
    chanPrefixes = initialChannelPrefixes cfg0
    defMonitors = initialDefaultMonitors cfg0
    defDisabled = initialDefaultDisabled cfg0
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
                           , _enabledMonitors = initialEnabledMonitors (H.lookupDefault H.empty b defDisabled)
                                                                       (H.lookupDefault [] b defMonitors)
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
  , let (pname, mname) = second (T.drop 1) (T.breakOn ":" monitor)
  ]

-- | Get the monitors disabled by default.
initialDefaultDisabled :: Table -> BackendMap (H.HashMap ChannelName [(PluginName, MonitorName)])
initialDefaultDisabled = buildBackendMap $ \cfg -> Just . H.fromList $
  [ (ChannelName cname, map toDisabled (toList mons))
  | (cname, VArray mons) <- maybe [] H.toList $ getTable "disabled" cfg
  ]
  where
    toDisabled (VString monitor) =
      let (pname, mname) = second (T.drop 1) (T.breakOn ":" monitor)
      in (PluginName pname, MonitorName mname)
    toDisabled _ = error "Malformed 'disabled' table! This should have been caught earlier than here!"

-- | Get the default commands.
initialDefaultCommands :: Table -> BackendMap [(PluginName, CommandName, Text)]
initialDefaultCommands = buildBackendMap $ \cfg -> Just
  [ (PluginName pname, CommandName cname, verb)
  | (verb, VString cmd) <- maybe [] H.toList $ getTable "commands" cfg
  , let (pname, cname) = second (T.drop 1) (T.breakOn ":" cmd)
  ]

-- | Get the deities.
initialDeities :: Table -> BackendMap [UserName]
initialDeities = buildBackendMap (Just . map UserName . getStrings "deities")

-- | Get the initial enabled monitors: this just populates the channel
-- entries where there are *disabled* monitors, as those are the ones
-- which deviate from the backend-default.
initialEnabledMonitors :: H.HashMap ChannelName [(PluginName, MonitorName)]
  -- ^ The disabled monitors.
  -> [(PluginName, MonitorName)]
  -- ^ The default-enabled monitors.
  -> H.HashMap (Maybe ChannelName) [(PluginName, MonitorName)]
initialEnabledMonitors disabled enabled = H.fromList
  [ (Just cname, enabled \\ disabledHere)
  | (cname, disabledHere) <- H.toList disabled
  ]

-------------------------------------------------------------------------------
-- Plugin

data OnOff = On | Off deriving (Eq, Ord, Read, Show, Enum, Bounded)

plugin :: BuiltinState -> config -> Either error Plugin
plugin state _ = Right Plugin
  { pluginHelp = "special built-in functionality"
  , pluginMonitors = H.empty
  , pluginCommands = H.fromList $
      map (\(cn, cf) -> (cn, wrapCommand cf state)) privileged ++
      map (\(cn, cf) -> (cn, cf state)) unprivileged
  }
  where
    privileged =
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

    unprivileged =
      [ ("help", help)
      ]

-- | Set the default prefix for a backend.
setDefaultPrefix :: BuiltinState -> Command
setDefaultPrefix st = Command
  { commandHelp = "\"word+\": set the default command prefix"
  , commandAction = \_ args ->
    modifyState st $ \state -> state & defaultPrefix .~ T.unwords args
  }

-- | Set the custom prefix for a channel. If applied outside of a
-- channel, this command does nothing.
setChannelPrefix :: BuiltinState -> Command
setChannelPrefix st = Command
  { commandHelp = "\"word+\": set the command prefix for this channel"
  , commandAction = \ev args ->
    modifyState st $ \state -> case eventChannel ev of
      Just c  -> state & channelPrefixes %~ H.insert c (T.unwords args)
      Nothing -> state
  }

-- | Unset the custom prefix for a channel. If applied outside of a
-- channel, this command does nothing.
unsetChannelPrefix :: BuiltinState -> Command
unsetChannelPrefix st = Command
  { commandHelp = "remove any custom command prefix for this channel"
  , commandAction = \ev _ ->
    modifyState st $ \state -> case eventChannel ev of
      Just c  -> state & channelPrefixes %~ H.delete c
      Nothing -> state
  }

-- | Enable or disable a plugin for a channel. If not in a channel,
-- this applies to whispers.
onOffPlugin :: OnOff -> BuiltinState -> Command
onOffPlugin mode st = Command
  { commandHelp = "\"plugin\": " <> (if mode == On then "enable" else "disable") <> " a plugin in this channel"
  , commandAction = \ev args -> do
    let plugins0 = map PluginName args
    plugins <- filterM isPlugin plugins0
    let go = if mode == On then filter (`notElem` plugins) else nub . (plugins++)
    modifyState st $ disabledPlugins . at (eventChannel ev) %~ (Just . go . fromMaybe [])
  }

-- | Enable or disable a monitor for a channel. If not in a channel,
-- this applies to whispers.
onOffMonitor :: OnOff -> BuiltinState -> Command
onOffMonitor mode st = Command
  { commandHelp = "\"(plugin:monitor)+\": " <> (if mode == On then "start" else "stop") <> " a monitor in this channel"
  , commandAction = \ev args -> do
    let monitors0 = [ (PluginName pn, MonitorName mn)
                    | arg <- args
                    , let (pn, mn) = second (T.drop 1) (T.breakOn ":" arg)
                    , not (T.null mn)
                    ]
    monitors <- filterM isMonitor monitors0
    let go = if mode == On then nub . (monitors++) else filter (`notElem` monitors)
    modifyState st $ enabledMonitors . at (eventChannel ev) %~ (Just . go . fromMaybe [])
  }

-- | Bind a command.
bindCommand :: BuiltinState -> Command
bindCommand st = Command
  { commandHelp = "\"plugin:command word+\": bind a verb to a command in this channel"
  , commandAction = \ev args -> case args of
    (cmd:vs) ->
      let (pname, cname) = second (T.drop 1) (T.breakOn ":" cmd)
          newVerb = T.unwords vs
          pcn@(pn,cn) = (PluginName pname, CommandName cname)
          go = ((pn, cn, newVerb):) . filter (\(_,_,verb) -> verb /= newVerb)
      in isCommand pcn >>= \b -> if not b || null vs
         then pure ()
         else modifyState st $ enabledCommands . at (eventChannel ev) %~ (Just . go . fromMaybe [])
    _ -> pure ()
  }

-- | Unbind a command.
unbindCommand :: BuiltinState -> Command
unbindCommand st = Command
  { commandHelp = "\"word+\": unbind a verb in this channel"
  , commandAction = \ev args ->
    let go = filter (\(_,_,vs) -> vs /= T.unwords args)
    in if null args
       then pure ()
       else modifyState st $ enabledCommands . at (eventChannel ev) %~ (Just . go . fromMaybe [])
  }

-- | Unbind all commands of the given type.
unbindAllCommand :: BuiltinState -> Command
unbindAllCommand st = Command
  { commandHelp = "\"(plugin:command)+\": unbind all verbs for a command in this channel"
  , commandAction = \ev args ->
    let commands = [ (PluginName pn, CommandName cn)
                   | arg <- args
                   , let (pn, cn) = second (T.drop 1) (T.breakOn ":" arg)
                   , not (T.null cn)
                   ]
        go = filter (\(cn,pn,_) -> (cn,pn) `notElem` commands)
    in modifyState st $ enabledCommands . at (eventChannel ev) %~ (Just . go . fromMaybe [])
  }

-- | Deify a list of users.
deify :: BuiltinState -> Command
deify st = Command
  { commandHelp = "\"name+\": elevate a user to godhood"
  , commandAction = \_ args ->
    let users = map UserName args
    in modifyState st $ deifiedUsers %~ (nub . (users++))
  }

-- | Un-deify a list of users.
degrade :: BuiltinState -> Command
degrade st = Command
  { commandHelp = "\"name+\": cast down a god"
  , commandAction = \_ args ->
    let users = map UserName args
    in modifyState st $ deifiedUsers %~ filter (`notElem` users)
  }

-- | Give help: in general, for a plugin, monitor, or command.
help :: BuiltinState -> Command
help st = Command
  { commandHelp = "display help text for a plugin, monitor, or command"
  , commandAction = \_ args -> do
      allPlugins <- liftF (GetInstance instPlugins)

      verbs    <- getVerbs    allPlugins
      plugins  <- getPlugins  allPlugins
      commands <- getCommands allPlugins
      monitors <- getMonitors allPlugins

      liftF . flip QuickReply () $ case args of
        -- Help for one plugin
        ("plugin":p:_)  -> showOne p plugins
        -- Help for one command
        ("command":c:_) -> showOne c commands
        -- Help for one monitor
        ("monitor":m:_) -> showOne m monitors
        -- List plugins
        ["plugin"]  -> showAll "plugin" plugins
        -- List commands
        ["command"] -> showAll "command" commands
        -- List monitors
        ["monitor"] -> showAll "monitor" monitors
        -- General help text
        [] -> showListOf verbs <> " (see also 'plugin', 'command', and 'monitor')"
        -- Verb
        _  -> showOne (T.unwords args) verbs
  }

  where
    getVerbs allPlugins = do
      allCommands <- getEnabledCommands st
      let toCHelp (pn, cn, verb) = (verb, (helpFor allPlugins pn (Right cn), True))
      pure $ map toCHelp allCommands

    getPlugins allPlugins = do
      disabled <- getDisabledPlugins st
      let toPHelp (pn, p) = (getPluginName pn, (pluginHelp p, pn `notElem` disabled))
      pure $ map toPHelp allPlugins

    getCommands allPlugins = do
      allCommands <- getEnabledCommands st
      let checkE _ _ [] = False
          checkE pn cn ((pn', cn', _):rest) = pn == pn' && cn == cn' || checkE pn cn rest
      let toCHelp (pn, cn, _) = ( getPluginName pn <> ":" <> getCommandName cn
                                , (helpFor allPlugins pn (Right cn), checkE pn cn allCommands))
      pure $ map toCHelp allCommands

    getMonitors allPlugins = do
      let allMonitors = [(pn, mn) | (pn, p) <- allPlugins, mn <- H.keys (pluginMonitors p)]
      enabled <- getEnabledMonitors st
      let toMHelp (pn, mn) = ( getPluginName pn <> ":" <> getMonitorName mn
                             , (helpFor allPlugins pn (Left mn), (pn,mn) `elem` enabled))
      pure $ map toMHelp allMonitors

    helpFor plugins pn key =
      let onError = fromMaybe . error . T.unpack
          plugin  = onError ("Help references missing plugin: " <> getPluginName pn) (lookup pn plugins)
      in case key of
           Right cn ->
             commandHelp . onError ("Help references missing command: " <> getPluginName pn <> ":" <> getCommandName cn) $
             H.lookup cn (pluginCommands plugin)
           Left  mn ->
             monitorHelp . onError ("Help references missing monitor: " <> getPluginName pn <> ":" <> getMonitorName mn) $
             H.lookup mn (pluginMonitors plugin)

    showAll thing things = thing <> "s: " <> showListOf things <> " (see also '" <> thing <> " <name>')"
    showOne key things = case lookup key things of
      Just (helpText, True)  -> key <> ": " <> helpText
      Just (helpText, False) -> key <> ": " <> helpText <> " [disabled]"
      Nothing -> "cannot find help for '" <> key <> "'"
    showListOf = T.intercalate ", " . sort . map fst

-------------------------------------------------------------------------------
-- Queries

-- | Get the command prefix in the current channel.
getCommandPrefix :: BuiltinState -> BackendM Text
getCommandPrefix st = getCommandPrefixIn st =<< liftF (GetEvent eventChannel)

-- | Get the command prefix in an arbitrary channel.
getCommandPrefixIn :: BuiltinState -> Maybe ChannelName -> BackendM Text
getCommandPrefixIn (BuiltinState statesVar) cname = do
  ib <- liftF (GetInstance id)

  liftIO . atomically $ do
    state <- bmLookup ib <$> readTVar statesVar
    let def = _defaultPrefix state
    pure $ case cname of
      Just cname' -> H.lookupDefault def cname' (_channelPrefixes state)
      Nothing -> def

-- | Get the disabled plugins in the current channel.
getDisabledPlugins :: BuiltinState -> BackendM [PluginName]
getDisabledPlugins st = getDisabledPluginsIn st =<< liftF (GetEvent eventChannel)

-- | Get the disabled plugins in an arbitrary channel.
getDisabledPluginsIn :: BuiltinState -> Maybe ChannelName -> BackendM [PluginName]
getDisabledPluginsIn (BuiltinState statesVar) cname = do
  ib <- liftF (GetInstance id)

  liftIO . atomically $ do
    state <- bmLookup ib <$> readTVar statesVar
    pure $ H.lookupDefault [] cname (_disabledPlugins state)

-- | Get the enabled commands in the current channel.
getEnabledCommands :: BuiltinState -> BackendM [(PluginName, CommandName, Text)]
getEnabledCommands st = getEnabledCommandsIn st =<< liftF (GetEvent eventChannel)

-- | Get the enabled commands in an arbitrary channel.
getEnabledCommandsIn :: BuiltinState -> Maybe ChannelName -> BackendM [(PluginName, CommandName, Text)]
getEnabledCommandsIn = getEnabledIn _enabledCommands _defaultCommands (\(pn,_,_) -> pn)

-- | Get the enabled monitors in the current channel.
getEnabledMonitors :: BuiltinState -> BackendM [(PluginName, MonitorName)]
getEnabledMonitors st = getEnabledMonitorsIn st =<< liftF (GetEvent eventChannel)

-- | Get the enabled monitors in an arbitrary channel.
getEnabledMonitorsIn :: BuiltinState -> Maybe ChannelName -> BackendM [(PluginName, MonitorName)]
getEnabledMonitorsIn = getEnabledIn _enabledMonitors _defaultMonitors fst

-- | Get the deities.
getDeities :: BuiltinState -> BackendM [UserName]
getDeities (BuiltinState statesVar) = do
  ib <- liftF (GetInstance id)

  liftIO . atomically $ do
    state <- bmLookup ib <$> readTVar statesVar
    pure (_deifiedUsers state)

-- | Check if a plugin exists.
isPlugin :: PluginName -> BackendM Bool
isPlugin pn = do
  allPlugins <- liftF (GetInstance instPlugins)
  pure $ any ((==pn) . fst) allPlugins

-- | Check if a monitor exists.
isMonitor :: (PluginName, MonitorName) -> BackendM Bool
isMonitor (pn, mn) = do
  allPlugins <- liftF (GetInstance instPlugins)
  pure $ case lookup pn allPlugins of
    Just p  -> H.member mn (pluginMonitors p)
    Nothing -> False

-- | Check if a command exists.
isCommand :: (PluginName, CommandName) -> BackendM Bool
isCommand (pn, cn) = do
  allPlugins <- liftF (GetInstance instPlugins)
  pure $ case lookup pn allPlugins of
    Just p  -> H.member cn (pluginCommands p)
    Nothing -> False

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
  setupDefault enabledMonitors _defaultMonitors
  setupDefault enabledCommands _defaultCommands

  where
    -- Set up default values, if unset.
    setupDefault :: Lens' BackendState (HashMap (Maybe ChannelName) a) -> (BackendState -> a) -> STM ()
    setupDefault l def = do
      state <- bmLookup ib <$> readTVar statesVar
      unless (H.member cname $ view l state) $ do
        let state' = state & l . at cname .~ Just (def state)
        modifyTVar statesVar (bmInsert ib state')

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

-- | Helper function for 'getEnabledCommandsIn' and
-- 'getEnabledMonitorsIn'.
getEnabledIn :: (BackendState -> HashMap (Maybe ChannelName) [a])
  -> (BackendState -> [a])
  -> (a -> PluginName)
  -> BuiltinState
  -> Maybe ChannelName
  -> BackendM [a]
getEnabledIn getMap getDef p st@(BuiltinState statesVar) cname = do
  ib       <- liftF (GetInstance id)
  dplugins <- getDisabledPlugins st

  liftIO . atomically $ do
    state <- bmLookup ib <$> readTVar statesVar
    case H.lookup cname (getMap state) of
      Just as -> pure (filter (\a -> p a `notElem` dplugins) as)
      Nothing -> do
        setupNewChannel st ib cname
        pure (getDef state)

-- | Check that the user of a command is a deity.
wrapCommand :: (BuiltinState -> Command) -> BuiltinState -> Command
wrapCommand cf st = let cmd = cf st in cmd { commandAction = \ev args -> do
  deities <- liftF (GetDeities id)
  if eventUser ev `elem` deities
    then commandAction cmd ev args
    else notDeityMessage >>= \msg -> liftF (Reply msg ())}

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
