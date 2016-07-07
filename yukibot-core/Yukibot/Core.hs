-- |
-- Module      : Yukibot.Core
-- Copyright   : (c) 2016 Michael Walker
-- License     : MIT
-- Stability   : experimental
-- Portability : portable
module Yukibot.Core
    ( module Yukibot.Backend
    , module Yukibot.Configuration
    , module Yukibot.Log
    , module Yukibot.Main
    , module Yukibot.Monad
    , module Yukibot.Types

    -- * Miscellaneous
    , privilegedCommand
    ) where

import Yukibot.Backend
import Yukibot.Configuration
import Yukibot.Log
import Yukibot.Main
import Yukibot.Monad
import Yukibot.Plugin.Builtin
import Yukibot.Types

{-# ANN module ("HLint: ignore Use import/export shortcut" :: String) #-}

-- | Make a command only available for deities.
privilegedCommand :: Command -> Command
privilegedCommand (Command cmd) = Command $ \ev args -> do
  d <- isDeified
  if d
    then cmd ev args
    else reply =<< notDeityMessage
