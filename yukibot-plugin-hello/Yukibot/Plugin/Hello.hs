{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Yukibot.Plugin.Hello
-- Copyright   : (c) 2016 Michael Walker
-- License     : MIT
-- Stability   : experimental
-- Portability : OverloadedStrings
--
-- A \"hello world\" plugin for yukibot-core.
module Yukibot.Plugin.Hello where

import Data.Maybe (fromMaybe)
import qualified Data.Text as T

import Yukibot.Core

helloPlugin :: Table -> Either a Plugin
helloPlugin cfg = Right (Plugin plugin) where
  msg = message cfg
  plugin (Event h (Just c) n m) | T.toLower m == "hello" =
    sendAction h $ Say c [n] msg
  plugin (Event h Nothing  n m) | T.toLower m == "hello" =
    sendAction h $ Whisper n msg
  plugin _ = pure ()

-- | Get the message from the configuration. Defaults to @"Hello!"@.
message :: Table -> T.Text
message = fromMaybe "Hello!" . getString "message"
