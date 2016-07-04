{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Yukibot.Plugin.Hello
-- Copyright   : (c) 2016 Michael Walker
-- License     : MIT
-- Stability   : experimental
-- Portability : OverloadedStrings
--
-- A \"hello world\" plugin for yukibot-core. This provides one
-- monitor and one command:
--
--     * Monitor "hello", responds with a greeting to any message
--       which consists only of "hello".
--
--     * Command "hello", which responds with a greeting.
module Yukibot.Plugin.Hello where

import qualified Data.HashMap.Strict as H
import Data.Maybe (fromMaybe)
import qualified Data.Text as T

import Yukibot.Core

helloPlugin :: Table -> Either a Plugin
helloPlugin cfg = Right Plugin
  { pluginMonitors = H.fromList [("hello", Monitor monitor)]
  , pluginCommands = H.fromList [("hello", Command command)]
  }

  where
    msg = message cfg

    monitor ev@(Event _ _ _ _ m) | T.toLower m == "hello" = command ev []
    monitor _ = pure ()

    command (Event h _ (Just c) n _) _ = sendAction h $ Say c [n] msg
    command (Event h _ Nothing  n _) _ = sendAction h $ Whisper n msg

-- | Get the message from the configuration. Defaults to @"Hello!"@.
message :: Table -> T.Text
message = fromMaybe "Hello!" . getString "message"
