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
module Yukibot.Backend.IRC (Channel, User, ConfigurationError(..), ircBackend) where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.STM
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (runReaderT)
import Data.Foldable (toList)
import qualified Data.HashMap.Strict as H
import Data.Maybe (mapMaybe)
import Data.Semigroup ((<>))
import Data.Text (Text, intercalate, pack)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Network.IRC.Client (ConnectionConfig, InstanceConfig, connect', connectWithTLS', defaultIRCConf, stdoutLogger)
import qualified Network.IRC.Client as IRC

import qualified Yukibot.Backend as Y
import qualified Yukibot.Configuration as Y

type Channel = Text
type User = Text

-- | A simple IRC backend.
--
-- TODO: Logging. Currently prints to stdout.
--
-- TODO: Reconnection.
--
-- TODO: Client-side timeout.
ircBackend :: Text -- ^ The hostname.
  -> Y.Table       -- ^ The configuration.
  -> Either ConfigurationError (Y.Backend Channel User)
ircBackend host cfg = case checkConfig host cfg of
  Left err   -> Left err
  Right desc -> Right Y.Backend
    { Y.initialise = connectToIrc host cfg
    , Y.run = handleIrc
    , Y.describe = desc
    }

-------------------------------------------------------------------------------
-- Configuration

-- | An error in the configuration
data ConfigurationError
  = MissingNick -- ^ The nick must be present and nonempty.
  | MissingPort -- ^ The port must be present and >0.
  | BadNickType -- ^ The nick must be a string.
  | BadPortType -- ^ The port must be an int.
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

-- | Check that the configuration is all ok, and return a description
-- if so.
checkConfig :: Text -> Y.Table -> Either ConfigurationError Text
checkConfig host cfg = case (H.lookup "nick" cfg, H.lookup "port" cfg) of
  (Just (Y.VString n), Just (Y.VInteger p)) | T.length n > 0 && p > 0 -> Right $
    let tls = if H.lookup "tls" cfg == Just (Y.VBoolean True) then "ssl://" else ""
        port = pack (show p)
    in "IRC <" <> tls <> n <> "@" <> host <> ":" <> port <> ">"
  (Just (Y.VString _), _) -> Left MissingNick
  (Nothing, _) -> Left MissingNick
  (Just _,  _) -> Left BadNickType
  (_, Just (Y.VInteger _)) -> Left MissingPort
  (_, Nothing) -> Left MissingPort
  (_, Just _)  -> Left BadPortType

-- | Get the nick from the configuation. Calls 'error' if missing.
getNick :: Y.Table -> Text
getNick cfg = case H.lookup "nick" cfg of
  Just (Y.VString nick) -> nick
  _ -> error "Missing nick!"

-- | Get the port from the configuration. Calls 'error' if missing.
getPort :: Integral i => Y.Table -> i
getPort cfg = case H.lookup "port" cfg of
  Just (Y.VInteger p) -> fromIntegral p
  _ -> error "Missing port!"

-- | Get the TLS flag from the configuration. Defaults to @False@.
getTls :: Y.Table -> Bool
getTls cfg = case H.lookup "tls" cfg of
  Just (Y.VBoolean b) -> b
  _ -> False

-- | Get the list of channels to join from the configuration. Defaults
-- to @[]@.
getChannels :: Y.Table -> [Channel]
getChannels cfg = case H.lookup "channels" cfg of
  Just (Y.VArray cs) -> mapMaybe (\case { Y.VString c -> Just c; _ -> Nothing }) (toList cs)
  _ -> []

-- | Get the server password from the configuration. Defaults to
-- @Nothing@.
getServerPassword :: Y.Table -> Maybe Text
getServerPassword cfg = case H.lookup "server-password" cfg of
  Just (Y.VString pass) | T.length pass > 0 -> Just pass
  _ -> Nothing

-- | Get the nickserv nick from the configuration. Defaults to
-- @"nickserv"@.
getNickserv :: Y.Table -> Text
getNickserv cfg = case H.lookup "nickserv" cfg of
  Just (Y.VString nick) | T.length nick > 0 -> nick
  _ -> "nickserv"

-- | Get the nickserv password from the configuration. Defaults to
-- @Nothing@.
getNickservPassword :: Y.Table -> Maybe Text
getNickservPassword cfg = case H.lookup "nickserv-password" cfg of
  Just (Y.VString pass) | T.length pass > 0 -> Just pass
  _ -> Nothing

-------------------------------------------------------------------------------
-- Backend main

type BackendState = (IRC.IRC () -> IO (), IO (), TVar Bool)

-- | Set up the IRC connection.
connectToIrc :: Text
  -> Y.Table
  -> ((Y.BackendHandle Channel User -> Y.Event Channel User) -> IO ())
  -> IO BackendState
connectToIrc host cfg receiveEvent = do
  cconf <- (if getTls cfg then connectWithTLS' else connect')
             stdoutLogger
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
-- Utilities

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
