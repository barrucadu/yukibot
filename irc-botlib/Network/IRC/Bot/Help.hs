{-# LANGUAGE OverloadedStrings #-}
module Network.IRC.Bot.Help (helpCmd) where

import Control.Concurrent.STM (atomically, readTVar)
import Control.Monad (liftM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.List (sort)
import Data.Monoid ((<>))
import Data.Text (Text, intercalate)

import Network.IRC.Bot.Commands.State
import Network.IRC.Bot.Utils

import qualified Data.Text as T

-- |Reply to the command with a list of all commands, or the
-- description of a command if one is given.
helpCmd :: CommandState s -> CommandDef s
helpCmd cs = CommandDef
  { _verb   = ["help"]
  , _help   = "[<command>] - Display a list of commands, or information about one command."
  , _action = go
  }

  where
    go [] _ ev = liftM (reply ev) $ listCommands cs
    go vs _ ev = liftM (reply ev) $ commandDesc cs vs

-- |Get a list of all commands.
listCommands :: MonadIO m => CommandState s -> m Text
listCommands cs = do
  commands <- liftIO . atomically . readTVar . _commandList $ cs
  return $ "Commands: " <> intercalate ", " (sort $ map (T.unwords . fst) commands)

-- |Get the description for a command.
commandDesc :: MonadIO m => CommandState s -> [Text] -> m Text
commandDesc cs verb = do
  commands <- liftIO . atomically . readTVar . _commandList $ cs
  return . maybe "I can't find that command" _help $ lookup verb commands
