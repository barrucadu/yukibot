{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Yukibot.Backend.IRC
-- Copyright   : (2) 2016 Michael Walker
-- License     : MIT
-- Stability   : experimental
-- Portability : LambdaCase, OverloadedStrings
--
-- An IRC backend for yukibot-core.
module Yukibot.Backend.IRC (ircBackend) where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.STM
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (runReaderT)
import Data.Semigroup ((<>))
import Data.Text (Text, intercalate)
import Network.IRC.Client (ConnectionConfig, InstanceConfig, connectWithTLS', defaultIRCConf, stdoutLogger)
import qualified Network.IRC.Client as IRC

import qualified Yukibot.Backend as Y

-- | A simple IRC backend.
--
-- TODO: Configuration. Currently the nick and network are hard-coded.
--
-- TODO: Logging. Currently prints to stdout.
--
-- TODO: Reconnection.
--
-- TODO: Client-side timeout.
ircBackend :: Y.Backend Text Text
ircBackend = Y.Backend
  { Y.initialise = connectToIrc
  , Y.run = handleIrc
  }

type BackendState = (ConnectionConfig (), InstanceConfig ())

-- | Set up the IRC connection.
connectToIrc :: IO BackendState
connectToIrc = do
  let hostname = "irc.freenode.net"
  let port = 7000
  let floodTimeout = 1
  let nick = "yukitest-2_0"

  cconf <- connectWithTLS' stdoutLogger hostname port floodTimeout

  let iconf  = defaultIRCConf nick

  pure (cconf, iconf)

-- | Connect to an IRC server and listen for events.
handleIrc :: (Y.Event Text Text -> IO ()) -> TQueue (Y.BackendAction Text Text) -> BackendState -> IO ()
handleIrc receiveEvent commandQueue (cconf, iconf) = do
  -- Fork off the client in its own thread
  stopvar <- atomically (newTVar False)
  let cconf' = cconf { IRC._ondisconnect = IRC._ondisconnect cconf >> liftIO (atomically $ writeTVar stopvar True) }
  let iconf' = iconf { IRC._eventHandlers = receiveHandler : IRC._eventHandlers iconf }
  state <- IRC.newIRCState cconf' iconf' ()
  tid <- forkIO (IRC.start' state)

  -- Process commands
  process (`runReaderT` state) (killThread tid) stopvar

  where
    -- Main loop: wait for commands (or for termination of the IRC
    -- client) and process them.
    process :: (IRC.IRC () -> IO ()) -> IO () -> TVar Bool -> IO ()
    process runIrc terminate stopvar = do
      act <- atomically $ do
        stopped <- readTVar stopvar
        action  <- tryReadTQueue commandQueue
        case (stopped, action) of
          (True, _) -> pure Nothing
          (False, Just a) -> pure (Just a)
          (False, Nothing) -> retry

      continue <- case act of
        Just (Y.Join channel) -> do
          runIrc . IRC.send $ IRC.Join channel
          pure True
        Just (Y.Leave channel) -> do
          runIrc . IRC.leaveChannel channel $ Just "Goodbye!"
          pure True
        Just (Y.Say channel users (Y.Message message)) -> do
          let who = if null users then "" else intercalate ", " users <> ": "
          runIrc . IRC.send $ IRC.Notice channel (Right $ who <> message)
          pure True
        Just (Y.Whisper user (Y.Message message)) -> do
          runIrc . IRC.send $ IRC.Notice user (Right message)
          pure True
        Just Y.Terminate -> terminate >> pure False
        Nothing -> pure False

      when continue (process runIrc terminate stopvar)

    -- Event handler for message reception.
    receiveHandler :: IRC.EventHandler s
    receiveHandler = IRC.EventHandler
      { IRC._description = "Receive PRIVMSGs and forward them to yukibot-core."
      , IRC._matchType = IRC.EPrivmsg
      , IRC._eventFunc = liftIO . dispatchEvent
      }

    -- Decode and send off an event.
    dispatchEvent :: IRC.UnicodeEvent -> IO ()
    dispatchEvent (IRC.Event _ (IRC.User nick) (IRC.Privmsg _ (Right msg))) =
      receiveEvent (Y.Event nick nick msg)
    dispatchEvent (IRC.Event _ (IRC.Channel chan nick) (IRC.Privmsg _ (Right msg))) =
      receiveEvent (Y.Event chan nick msg)
    dispatchEvent _ = pure ()
