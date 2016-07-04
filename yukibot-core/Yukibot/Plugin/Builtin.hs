{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Yukibot.Plugin.Builtin
-- Copyright   : (c) 2016 Michael Walker
-- License     : MIT
-- Stability   : experimental
-- Portability : LambdaCase, OverloadedStrings
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
--     * "disable-plugin", disable a plugin in this channel.
--
--     * "enable-plugin", enable a previously-disabled plugin in this
--       channel. Monitors and commands retain their prior
--       enabled/disabled state.
--
--     * "disable-monitor", disable a monitor in this channel.
--
--     * "enable-monitor", enable a previously-disabled monitor in
--       this channel.
--
--     * "disable-command", disable a command in this channel.
--
--     * "enable-command", enable a previously-disabled command in
--       this channel.
module Yukibot.Plugin.Builtin
  ( -- * State
    BuiltinState
  , initialBuiltinState

  -- * Plugin
  , builtinPlugin

  -- * Actions
  , builtinGetPrefix
  , builtinIsMonitorEnabled
  , builtinIsCommandEnabled
  ) where

import Control.Concurrent.STM (TVar, atomically, newTVar, modifyTVar, readTVar)
import Data.Foldable (toList)
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import qualified Data.HashMap.Strict as H
import qualified Data.HashSet as HS
import Data.Text (Text)
import qualified Data.Text as T

import Yukibot.Configuration
import Yukibot.Types

-------------------------------------------------------------------------------
-- State

-- | Abstract mutable state.
data BuiltinState = BuiltinState
  { defaultPrefixes  :: TVar (HashMap (BackendName, Text, Int) Text)
  , channelPrefixes  :: TVar (HashMap (BackendName, Text, Int, ChannelName) Text)
  , disabledPlugins  :: TVar (HashSet (BackendName, Text, Int, ChannelName, PluginName))
  , disabledMonitors :: TVar (HashSet (BackendName, Text, Int, ChannelName, PluginName, MonitorName))
  , disabledCommands :: TVar (HashSet (BackendName, Text, Int, ChannelName, PluginName, CommandName))
  }

-- | Create the initial state.
initialBuiltinState :: Table -> IO BuiltinState
initialBuiltinState cfg = atomically $
  BuiltinState <$> newTVar (getDefaultPrefixes cfg)
               <*> newTVar (getChannelPrefixes cfg)
               <*> newTVar HS.empty
               <*> newTVar HS.empty
               <*> newTVar HS.empty

-- | Get the default prefixes.
getDefaultPrefixes :: Table -> HashMap (BackendName, Text, Int) Text
getDefaultPrefixes cfg0 = H.fromList [ ((BackendName bname, sname, index), prefix)
                                     | (bname, VTable cfgs) <- maybe [] H.toList (getTable "backend" cfg0)
                                     , (sname, cfg)         <- H.toList cfgs
                                     , (index, Just prefix) <- prefixesOf cfg
                                     ]
  where
    prefixesOf (VTable  c)  = [(0, get (c `override` cfg0))]
    prefixesOf (VTArray cs) = [(i, get (c `override` cfg0)) | (i, c) <- zip [0..] (toList cs)]
    prefixesOf _ = [(0, get cfg0)]

    get = getString "default-prefix"

-- | Get the channel prefixes.
getChannelPrefixes :: Table -> HashMap (BackendName, Text, Int, ChannelName) Text
getChannelPrefixes cfg0 = H.fromList [ ((BackendName bname, sname, index, ChannelName cname), prefix)
                                     | (bname, VTable cfgs)    <- maybe [] H.toList (getTable "backend" cfg0)
                                     , (sname, cfg)            <- H.toList cfgs
                                     , (index, chans)          <- prefixesOf cfg
                                     , (cname, VString prefix) <- chans
                                     ]
  where
    prefixesOf (VTable  c)  = [(0, get (c `override` cfg0))]
    prefixesOf (VTArray cs) = [(i, get (c `override` cfg0)) | (i, c) <- zip [0..] (toList cs)]
    prefixesOf _ = [(0, get cfg0)]

    get = maybe [] H.toList . getTable "channel-prefixes"

-------------------------------------------------------------------------------
-- Plugin

builtinPlugin :: BuiltinState -> config -> Either error Plugin
builtinPlugin state _ = Right Plugin
  { pluginMonitors = H.empty
  , pluginCommands = H.fromList [ ("set-default-prefix",   setDefaultPrefix   state)
                                , ("set-channel-prefix",   setChannelPrefix   state)
                                , ("unset-channel-prefix", unsetChannelPrefix state)
                                , ("disable-plugin",       onOffPlugin        state False)
                                , ("enable-plugin",        onOffPlugin        state True)
                                , ("disable-monitor",      onOffMonitor       state False)
                                , ("enable-monitor",       onOffMonitor       state True)
                                , ("disable-command",      onOffCommand       state False)
                                , ("enable-command",       onOffCommand       state True)
                                ]
  }

-- | Set the default prefix for a backend.
setDefaultPrefix :: BuiltinState -> Command
setDefaultPrefix st = Command $ \ev args -> do
  let newPrefix = T.unwords args
  let addr = backendAddress ev
  atomically $ modifyTVar (defaultPrefixes st) (H.insert addr newPrefix)

-- | Set the custom prefix for a channel. If applied outside of a
-- channel, this command does nothing.
setChannelPrefix :: BuiltinState -> Command
setChannelPrefix st = Command $ \ev args -> do
  let newPrefix = T.unwords args
  let (bname, sname, index) = backendAddress ev
  atomically . whenJust (eventChannel ev) $ \cname ->
    modifyTVar (channelPrefixes st) $ H.insert (bname, sname, index, cname) newPrefix

-- | Unset the custom prefix for a channel. If applied outside of a
-- channel, this command does nothing.
unsetChannelPrefix :: BuiltinState -> Command
unsetChannelPrefix st = Command $ \ev _ -> do
  let (bname, sname, index) = backendAddress ev
  atomically . whenJust (eventChannel ev) $ \cname ->
    modifyTVar (channelPrefixes st) $ H.delete (bname, sname, index, cname)

-- | Enable or disable a plugin for a channel. If applied outside of
-- a channel, this command does nothing.
onOffPlugin :: BuiltinState -> Bool -> Command
onOffPlugin st = onOffThing (disabledPlugins st) $
  \bname sname index cname arg -> Just (bname, sname, index, cname, PluginName arg)

-- | Enable or disable a monitor for a channel. If applied outside of
-- a channel, this command does nothing.
onOffMonitor :: BuiltinState -> Bool -> Command
onOffMonitor st = onOffThing (disabledMonitors st) $ \bname sname index cname arg -> case toMon arg of
  Just (pn, mn) -> Just (bname, sname, index, cname, pn, mn)
  Nothing -> Nothing

  where
    toMon arg = case T.breakOn ":" arg of
      (pn, mn) | T.null mn -> Nothing
               | otherwise -> Just (PluginName pn, MonitorName $ T.tail mn)

-- | Enable or disable a command for a channel. If applied outside of
-- a channel, this command does nothing.
onOffCommand :: BuiltinState -> Bool -> Command
onOffCommand st = onOffThing (disabledCommands st) $ \bname sname index cname arg -> case toCmd arg of
  Just (pn, mn) -> Just (bname, sname, index, cname, pn, mn)
  Nothing -> Nothing

  where
    toCmd arg = case T.breakOn ":" arg of
      (pn, cn) | T.null cn -> Nothing
               | otherwise -> Just (PluginName pn, CommandName $ T.tail cn)

-- | Enable or disable a <thing> for a channel. If applied outside of
-- a channel, this command does nothing.
onOffThing :: (Eq x, Hashable x)
  => TVar (HashSet x)
  -> (BackendName -> Text -> Int -> ChannelName -> Text -> Maybe x)
  -> Bool
  -> Command
onOffThing var f enable = Command $ \ev args -> atomically . whenJust (eventChannel ev) $ \cname -> do
  let (bname, sname, index) = backendAddress ev
  let vals = HS.fromList [x | arg <- args, let Just x = f bname sname index cname arg]
  modifyTVar var $ \s -> (if enable then difference else HS.union) s vals

  where
    difference l r = HS.filter (not . (`HS.member` r)) l

-------------------------------------------------------------------------------
-- Queries

-- | Get the prefix for a channel.
builtinGetPrefix :: BuiltinState -> BackendName -> Text -> Int -> Maybe ChannelName -> IO Text
builtinGetPrefix st bname sname index mcname = atomically $ do
  defaultPrefix <- H.lookupDefault "!" (bname, sname, index) <$> readTVar (defaultPrefixes st)
  case mcname of
    Just cname ->
      H.lookupDefault defaultPrefix (bname, sname, index, cname) <$> readTVar (channelPrefixes st)
    Nothing -> pure defaultPrefix

-- | Check if a monitor is enabled.
builtinIsMonitorEnabled :: BuiltinState -> BackendName -> Text -> Int -> ChannelName -> PluginName -> MonitorName -> IO Bool
builtinIsMonitorEnabled st bname sname index cname pn mn = atomically $ do
  p <- HS.member (bname, sname, index, cname, pn)     <$> readTVar (disabledPlugins  st)
  m <- HS.member (bname, sname, index, cname, pn, mn) <$> readTVar (disabledMonitors st)
  pure . not $ p || m

-- | Check if a command is enabled.
builtinIsCommandEnabled :: BuiltinState -> BackendName -> Text -> Int -> ChannelName -> PluginName -> CommandName -> IO Bool
builtinIsCommandEnabled st bname sname index cname pn cn = atomically $ do
  p <- HS.member (bname, sname, index, cname, pn)     <$> readTVar (disabledPlugins  st)
  c <- HS.member (bname, sname, index, cname, pn, cn) <$> readTVar (disabledCommands st)
  pure . not $ p || c

-------------------------------------------------------------------------------
-- Utilities

-- | Apply a monadic function when a value is @Just@.
whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust (Just a) f = f a
whenJust Nothing  _ = pure ()

-- | Get the unique \"address\" of a backend.
backendAddress :: Event -> (BackendName, Text, Int)
backendAddress ev =
  let bname = backendName  $ eventHandle ev
      sname = specificName $ eventHandle ev
      index = backendIndex $ eventHandle ev
  in (bname, sname, index)
