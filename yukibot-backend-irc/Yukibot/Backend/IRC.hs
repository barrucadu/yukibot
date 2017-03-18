{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Yukibot.Backend.IRC
-- Copyright   : (c) 2016 Michael Walker
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
import Data.ByteString (ByteString)
import Data.Semigroup ((<>))
import Data.Text (Text, intercalate, unpack)
import Data.Text.Encoding (encodeUtf8)
import Network.IRC.Client (connect', connectWithTLS', defaultIRCConf)
import qualified Network.IRC.Client as IRC
import System.FilePath ((<.>))

import qualified Yukibot.Core as Y

import Yukibot.Backend.IRC.Configuration

-- | A simple IRC backend.
--
-- TODO: Reconnection.
--
-- TODO: Client-side timeout.
ircBackend :: Text  -- ^ The hostname.
  -> Table          -- ^ The configuration.
  -> Either Text Y.Backend
ircBackend host cfg = case checkConfig host cfg of
  Left err   -> Left err
  Right desc ->
    let logFileName = unpack $ "irc-" <> host <> "-" <> getNick cfg
    in Right Y.Backend
       { Y.initialise = connectToIrc host cfg
       , Y.run = handleIrc
       , Y.describe = desc
       -- The log file names are overridden by the core if specified
       -- in the config, so these are just defaults.
       , Y.logFile = logFileName <.> "log"
       }

-------------------------------------------------------------------------------
-- Backend main

type BackendState = (IRC.IRC Bool -> IO Bool, IO (), TVar Bool)

-- | Set up the IRC connection.
connectToIrc :: Text
  -> Table
  -> Y.Logger
  -> ((Y.BackendHandle -> Y.Event) -> IO ())
  -> IO BackendState
connectToIrc host cfg logger receiveEvent = do
  cconf <- (if getTLS cfg then connectWithTLS' else connect')
             (botLogger logger)
             (encodeUtf8 host)
             (getPort cfg)
             1
  let iconf = (defaultIRCConf $ getNick cfg)
                { IRC._channels = getChannels cfg
                , IRC._password = getServerPassword cfg
                }

  -- Fork off the client in its own thread
  (stopvar, stopper) <- atomically newFlag
  let cconf' = cconf { IRC._ondisconnect = IRC._ondisconnect cconf >> liftIO (atomically stopper) }

  (welcomevar, welcomer) <- atomically (newWelcomeHandler (getNickserv cfg) (getNickservPassword cfg))
  let receiver = receiveHandler receiveEvent
  let iconf' = iconf { IRC._eventHandlers = receiver : welcomer : IRC._eventHandlers iconf }
  state <- IRC.newIRCState cconf' iconf' ()
  tid <- forkIO (IRC.start' state)

  -- Block until 001 (numeric welcome)
  atomically (readTVar welcomevar >>= check)

  -- Return the backend state
  pure ((`runReaderT` state), killThread tid, stopvar)

-- | Wait for commands.
handleIrc :: TQueue Y.Action -> BackendState -> IO ()
handleIrc commandQueue (runIrc, terminate, stopvar) = process where
  -- Main loop: wait for commands (or for termination of the IRC
  -- client) and process them.
  process :: IO ()
  process = do
    -- Wait for an action, or termination of the client thread.
    act <- atomically $ do
      stopped <- readTVar stopvar
      action  <- tryReadTQueue commandQueue
      case (stopped, action) of
        (True, _) -> pure Nothing
        (False, Just a) -> pure (Just a)
        (False, Nothing) -> retry

    -- Process the action
    continue <- runIrc $ case act of
      Just (Y.Join channel) -> do
        IRC.send $ IRC.Join (Y.getChannelName channel)
        pure True
      Just (Y.Leave channel) -> do
        IRC.leaveChannel (Y.getChannelName channel) $ Just "Goodbye!"
        pure True
      Just (Y.Say channel users message) -> do
        let who = if null users then "" else intercalate ", " (map Y.getUserName users) <> ": "
        IRC.send $ IRC.Notice (Y.getChannelName channel) (Right $ who <> message)
        pure True
      Just (Y.Whisper user message) -> do
        IRC.send $ IRC.Notice (Y.getUserName user) (Right message)
        pure True
      _ -> pure False

    -- Loop or stop.
    when continue process
    terminate

-------------------------------------------------------------------------------
-- Utilities

-- | Create a new STM flag: writes 'True' to the returned 'TVar' when the action is run.
newFlag :: STM (TVar Bool, STM ())
newFlag = do
  flagvar <- newTVar False
  pure (flagvar, writeTVar flagvar True)

-- | Log raw messages.
botLogger :: Y.Logger -> IRC.Origin -> ByteString -> IO ()
botLogger logger IRC.FromClient = Y.toServer   logger
botLogger logger IRC.FromServer = Y.fromServer logger

-- | Event handler for message reception.
receiveHandler :: ((Y.BackendHandle -> Y.Event) -> IO ()) -> IRC.EventHandler s
receiveHandler receiveEvent = IRC.EventHandler
  { IRC._description = "Receive PRIVMSGs and forward them to yukibot-core."
  , IRC._matchType = IRC.EPrivmsg
  , IRC._eventFunc = \ev -> do
    user <- Y.UserName . IRC._nick <$> IRC.instanceConfig
    liftIO $ dispatchEvent user ev
  }
   where
    -- Decode and send off an event.
    dispatchEvent user (IRC.Event _ (IRC.User nick) (IRC.Privmsg _ (Right msg))) =
      receiveEvent (\h -> Y.Event h user Nothing (Y.UserName nick) msg)
    dispatchEvent user (IRC.Event _ (IRC.Channel chan nick) (IRC.Privmsg _ (Right msg))) =
      receiveEvent (\h -> Y.Event h user (Just $ Y.ChannelName chan) (Y.UserName nick) msg)
    dispatchEvent _ _ = pure ()

-- | Flag and handler for 001 (numeric welcome). Flag is set to 'True'
-- on receipt.
newWelcomeHandler :: Text -> Maybe Text -> STM (TVar Bool, IRC.EventHandler s)
newWelcomeHandler nickserv nickservPassword = do
    (flag, setFlag) <- newFlag
    let handler = IRC.EventHandler
          { IRC._description = "Set a flag on 001 (numeric welcome)"
          , IRC._matchType = IRC.ENumeric
          , IRC._eventFunc = \case
              { IRC.Event _ _ (IRC.Numeric num _) | num == 001 -> do
                -- Auth with nickserv
                case nickservPassword of
                  Just p -> IRC.send . IRC.Privmsg nickserv . Right $ "IDENTIFY " <> p
                  Nothing -> pure ()
                -- Set the flag
                liftIO (atomically setFlag)
              ; _ -> pure ()
              }
          }
    pure (flag, handler)
