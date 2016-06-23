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
module Yukibot.Backend.IRC (Channel, User, ircBackend) where

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

type Channel = Text
type User = Text

-- | A simple IRC backend.
--
-- TODO: Configuration. Currently the nick and network are hard-coded.
--
-- TODO: Logging. Currently prints to stdout.
--
-- TODO: Reconnection.
--
-- TODO: Client-side timeout.
ircBackend :: Y.Backend Channel User
ircBackend = Y.Backend
  { Y.initialise = connectToIrc
  , Y.run = handleIrc
  , Y.describe = "IRC <ssl://irc.freenode.net:7000>"
  }

type BackendState = (IRC.IRC () -> IO (), IO (), TVar Bool)

-- | Set up the IRC connection.
connectToIrc :: ((Y.BackendHandle Channel User -> Y.Event Channel User) -> IO ())
  -> IO BackendState
connectToIrc receiveEvent = do
  let hostname = "irc.freenode.net"
  let port = 7000
  let floodTimeout = 1
  let nick = "yukitest-2_0"

  cconf <- connectWithTLS' stdoutLogger hostname port floodTimeout
  let iconf  = defaultIRCConf nick

  -- Fork off the client in its own thread
  (stopvar, stopper) <- atomically newFlag
  let cconf' = cconf { IRC._ondisconnect = IRC._ondisconnect cconf >> liftIO (atomically stopper) }

  (welcomevar, welcomer) <- atomically newWelcomeHandler
  let receiver = receiveHandler receiveEvent
  let iconf' = iconf { IRC._eventHandlers = receiver : welcomer : IRC._eventHandlers iconf }
  state <- IRC.newIRCState cconf' iconf' ()
  tid <- forkIO (IRC.start' state)

  -- Block until 001 (numeric welcome)
  atomically (readTVar welcomevar >>= check)

  -- Return the backend state
  pure ((`runReaderT` state), killThread tid, stopvar)

-- | Connect to an IRC server and listen for events.
handleIrc :: TQueue (Y.BackendAction Channel User) -> BackendState -> IO ()
handleIrc commandQueue (runIrc, terminate, stopvar) = process where
  -- Main loop: wait for commands (or for termination of the IRC
  -- client) and process them.
  process :: IO ()
  process = do
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

    when continue process

-------------------------------------------------------------------------------

-- | Create a new STM flag: writes 'True' to the returned 'TVar' when the action is run.
newFlag :: STM (TVar Bool, STM ())
newFlag = do
  flagvar <- newTVar False
  pure (flagvar, writeTVar flagvar True)

-- | Event handler for message reception.
receiveHandler :: ((Y.BackendHandle Channel User -> Y.Event Channel User) -> IO ()) -> IRC.EventHandler s
receiveHandler receiveEvent = IRC.EventHandler
  { IRC._description = "Receive PRIVMSGs and forward them to yukibot-core."
  , IRC._matchType = IRC.EPrivmsg
  , IRC._eventFunc = liftIO . dispatchEvent
  }
   where
    -- Decode and send off an event.
    dispatchEvent :: IRC.UnicodeEvent -> IO ()
    dispatchEvent (IRC.Event _ (IRC.User nick) (IRC.Privmsg _ (Right msg))) =
      receiveEvent (\h -> Y.Event h Nothing nick msg)
    dispatchEvent (IRC.Event _ (IRC.Channel chan nick) (IRC.Privmsg _ (Right msg))) =
      receiveEvent (\h -> Y.Event h (Just chan) nick msg)
    dispatchEvent _ = pure ()

-- | Flag and handler for 001 (numeric welcome). Flag is set to 'True'
-- on receipt.
newWelcomeHandler :: STM (TVar Bool, IRC.EventHandler s)
newWelcomeHandler = do
    (flag, setFlag) <- newFlag
    let handler = IRC.EventHandler
          { IRC._description = "Set a flag on 001 (numeric welcome)"
          , IRC._matchType = IRC.ENumeric
          , IRC._eventFunc = \case
              { IRC.Event _ _ (IRC.Numeric num _) | num == 001 -> liftIO (atomically setFlag)
              ; _ -> pure ()
              }
          }
    pure (flag, handler)
