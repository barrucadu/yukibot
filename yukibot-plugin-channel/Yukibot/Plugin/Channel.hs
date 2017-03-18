{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Yukibot.Plugin.Channel
-- Copyright   : (c) 2016 Michael Walker
-- License     : MIT
-- Stability   : experimental
-- Portability : OverloadedStrings
--
-- A channel management plugin for yukibot-core. This provides the
-- following commands:
--
--     * "join", joins the given channels, one per arg.
--
--     * "leave", leaves the current channel.
--
-- Both of these commands require the user to be a deity.
module Yukibot.Plugin.Channel where

import qualified Data.HashMap.Strict as H

import Yukibot.Core
import Yukibot.Utils

channelPlugin :: config -> Either error Plugin
channelPlugin _ = Right Plugin
  { pluginHelp = "channel management"
  , pluginMonitors = H.empty
  , pluginCommands = H.fromList [ ("join",  joinCommand)
                                , ("leave", leaveCommand)
                                ]
  }

-- | Join a collection of channels.
joinCommand :: Command
joinCommand = privilegedCommand Command
  { commandHelp = "\"channel+\": join a channel"
  , commandAction = \_ args -> mapM_ (joinChannel . ChannelName) args
  }

-- | Leave the current channel.
leaveCommand :: Command
leaveCommand = privilegedCommand Command
  { commandHelp = "leave the current channel"
  , commandAction = \ev _ -> maybe (pure ()) leaveChannel (eventChannel ev)
  }
