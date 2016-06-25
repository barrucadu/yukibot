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
module Yukibot.Backend.IRC (Channel, User, ircBackend) where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.STM
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (runReaderT)
import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import Data.Semigroup ((<>))
import Data.Text (Text, intercalate, unpack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Network.IRC.Client (connect', connectWithTLS', defaultIRCConf)
import qualified Network.IRC.Client as IRC
import System.FilePath ((<.>))

import qualified Yukibot.Backend as Y
import qualified Yukibot.Log as Y

import Yukibot.Backend.IRC.Configuration

type Channel = Text
type User = Text

-- | A simple IRC backend.
--
-- TODO: Reconnection.
--
-- TODO: Client-side timeout.
ircBackend :: Text  -- ^ The hostname.
  -> Table          -- ^ The configuration.
  -> Either Text (Y.Backend Channel User)
ircBackend host cfg = case checkConfig host cfg of
  Left err   -> Left err
  Right desc -> let logger = ircLogger host cfg in Right Y.Backend
    { Y.initialise = connectToIrc logger host cfg
    , Y.run = handleIrc logger
    , Y.describe = desc
    }

-- | A simple logger: logs to the filename in the config if given,
-- uses hostname if not.
ircLogger :: Text -> Table -> Y.Logger Channel User
ircLogger host cfg = Y.logger id id rawLog unrawLog where
  rawLog   = fromMaybe (unpack host <.> "raw.log") (getRawLogFile cfg)
  unrawLog = fromMaybe (unpack host <.> "log")     (getLogFile    cfg)

-------------------------------------------------------------------------------
-- Backend main

type BackendState = (IRC.IRC Bool -> IO Bool, IO (), TVar Bool)

-- | Set up the IRC connection.
connectToIrc :: Y.Logger Channel User
  -> Text
  -> Table
  -> ((Y.BackendHandle Channel User -> IO (Y.Event Channel User)) -> IO ())
  -> IO BackendState
connectToIrc logger host cfg receiveEvent = do
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
  let receiver = receiveHandler logger receiveEvent
  let iconf' = iconf { IRC._eventHandlers = receiver : welcomer : IRC._eventHandlers iconf }
  state <- IRC.newIRCState cconf' iconf' ()
  tid <- forkIO (IRC.start' state)

  -- Block until 001 (numeric welcome)
  atomically (readTVar welcomevar >>= check)

  -- Return the backend state
  pure ((`runReaderT` state), killThread tid, stopvar)

-- | Connect to an IRC server and listen for events.
handleIrc :: Y.Logger Channel User
  -> TQueue (Y.BackendAction Channel User)
  -> BackendState
  -> IO ()
handleIrc logger commandQueue (runIrc, terminate, stopvar) = process where
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

    -- Log the action
    maybe (pure ()) (Y.loggerAction logger) act

    -- Process the action
    continue <- runIrc $ case act of
      Just (Y.Join channel) -> do
        IRC.send $ IRC.Join channel
        pure True
      Just (Y.Leave channel) -> do
        IRC.leaveChannel channel $ Just "Goodbye!"
        pure True
      Just (Y.Say channel users (Y.Message message)) -> do
        let who = if null users then "" else intercalate ", " users <> ": "
        IRC.send $ IRC.Notice channel (Right $ who <> message)
        pure True
      Just (Y.Whisper user (Y.Message message)) -> do
        IRC.send $ IRC.Notice user (Right message)
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
botLogger :: Y.Logger Channel User -> IRC.Origin -> ByteString -> IO ()
botLogger logger IRC.FromClient = Y.loggerToServer   logger . decodeUtf8
botLogger logger IRC.FromServer = Y.loggerFromServer logger . decodeUtf8

-- | Event handler for message reception.
receiveHandler :: Y.Logger Channel User
  -> ((Y.BackendHandle Channel User -> IO (Y.Event Channel User)) -> IO ())
  -> IRC.EventHandler s
receiveHandler logger receiveEvent = IRC.EventHandler
  { IRC._description = "Receive PRIVMSGs and forward them to yukibot-core."
  , IRC._matchType = IRC.EPrivmsg
  , IRC._eventFunc = liftIO . dispatchEvent
  }
   where
    -- Decode and send off an event.
    dispatchEvent :: IRC.UnicodeEvent -> IO ()
    dispatchEvent (IRC.Event _ (IRC.User nick) (IRC.Privmsg _ (Right msg))) =
      receiveEvent (\h -> let e = Y.Event h Nothing nick msg in Y.loggerEvent logger e >> pure e)
    dispatchEvent (IRC.Event _ (IRC.Channel chan nick) (IRC.Privmsg _ (Right msg))) =
      receiveEvent (\h -> let e = Y.Event h (Just chan) nick msg in Y.loggerEvent logger e >> pure e)
    dispatchEvent _ = pure ()

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
