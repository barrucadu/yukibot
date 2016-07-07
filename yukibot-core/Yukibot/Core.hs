-- |
-- Module      : Yukibot.Core
-- Copyright   : (c) 2016 Michael Walker
-- License     : MIT
-- Stability   : experimental
-- Portability : portable
module Yukibot.Core
    ( module Yukibot.Configuration
    , module Yukibot.Main
    , module Yukibot.Monad
    , module Yukibot.Types

    -- * Miscellaneous
    , privilegedCommand
    ) where

import Yukibot.Configuration
import Yukibot.Main
import Yukibot.Monad
import Yukibot.Plugin.Builtin
import Yukibot.Types

{-# ANN module ("HLint: ignore Use import/export shortcut" :: String) #-}

-- | Make a command only available for deities.
privilegedCommand :: Command -> Command
privilegedCommand cmd = cmd { commandAction = \ev args -> do
  d <- isDeified
  if d
    then commandAction cmd ev args
    else reply =<< notDeityMessage}
