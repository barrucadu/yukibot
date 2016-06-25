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

import qualified Data.Text as T

import Yukibot.Core

helloPlugin :: Plugin
helloPlugin = Plugin plugin where
  plugin (Event h (Just c) n m) | T.toLower m == "hello" =
    sendAction h $ Say c [n] "Hello!"
  plugin (Event h Nothing  n m) | T.toLower m == "hello" =
    sendAction h $ Whisper n "Hello!"
  plugin _ = pure ()
