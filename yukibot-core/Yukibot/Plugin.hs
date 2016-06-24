{-# LANGUAGE RankNTypes #-}

-- |
-- Module      : Yukibot.Plugin
-- Copyright   : (c) 2016 Michael Walker
-- License     : MIT
-- Stability   : experimental
-- Portability : RankNTypes
--
-- TODO: Configuration.
--
-- TODO: Blacklisting.
module Yukibot.Plugin where

import Yukibot.Backend (Event)

newtype Plugin = Plugin (forall channel user. Event channel user -> IO ())
