{-# LANGUAGE OverloadedStrings #-}

-- TODO: Catch SIGTERM and disconnect from all networks.
module Main where

import Control.Applicative ((<$>))
import Control.Monad       (void)
import Network.IRC.Asakura
import Network.IRC.Asakura.Types
import Network.IRC.IDTE
import System.Directory    (doesFileExist)
import System.Exit         (exitFailure)
import Yukibot.State

import qualified Network.IRC.Asakura.Commands as C
import qualified Yukibot.Plugins.LinkInfo     as L

-- |Load the configuration file, if it exists, otherwise initialise a
-- new state. Upon successfully constructing a state, run the bot.
main :: IO ()
main = do
  confExists <- doesFileExist "yukibot.json"
  ys <- if confExists
       then stateFromFile "yukibot.json"
       else Just <$> initialise

  case ys of
    Just ys' -> runWithState ys'
    Nothing  -> putStrLn "Failed to parse configuration file." >> exitFailure

-- |Run the bot with a given state.
runWithState :: YukibotState -> IO ()
runWithState ys = do
  cconf <- connect "irc.freenode.net" 6667
  state <- newBotState

  -- Start commands
  addGlobalEventHandler' state $ C.eventRunner $ _commandState ys

  -- Start LinkInfo
  addGlobalEventHandler' state L.eventHandler

  case cconf of
    Right cconf' -> do
      void $ run cconf' (defaultIRCConf "yukibot") state
      save "yukibot.json" ys

    Left err -> putStrLn err >> exitFailure
