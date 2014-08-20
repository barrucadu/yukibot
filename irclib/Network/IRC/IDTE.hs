{-# LANGUAGE OverloadedStrings #-}

-- |Entry point to the Integrated Data Thought Entity.
module Network.IRC.IDTE
    ( module Network.IRC.IDTE.Types
    , module Network.IRC.IDTE.Messages
    , connect
    , connectWithTLS
    , connectWithTLS'
    , run
    , send
    , disconnect
    , defaultIRCConf
    , setNick
    ) where

import Control.Applicative    ((<$>))
import Control.Concurrent     (threadDelay)
import Control.Monad          (forever, when, void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Reader (runReaderT)
import Control.Monad.Trans.State  (runStateT)
import Data.Char              (isAlphaNum)
import Data.Maybe             (fromMaybe)
import Data.Monoid            ((<>))
import Data.Text              (Text, breakOn, takeEnd, toUpper, unpack)
import Data.Text.Encoding     (decodeUtf8)
import Data.Time.Calendar     (fromGregorian)
import Data.Time.Clock        (UTCTime(..), addUTCTime, diffUTCTime, getCurrentTime)
import Data.Time.Format       (formatTime)
import Network
import Network.IRC            (Message, encode)
import Network.IRC.IDTE.Events (toEvent)
import Network.IRC.IDTE.Messages
import Network.IRC.IDTE.Types
import Network.TLS            (Cipher)
import System.Locale          (defaultTimeLocale)

import qualified Data.Text            as T
import qualified Network.IRC.IDTE.Net as N

-- *Connecting to an IRC network

-- |Connect to a server without TLS.
connect :: MonadIO m => HostName -> Int -> m (Either String ConnectionConfig)
connect host port = do
  res <- N.connect host port

  return $
    case res of
      Right sock -> Right ConnectionConfig
                     { _socket = sock
                     , _tls    = Nothing
                     , _server = host
                     , _port   = port
                     }
      Left err -> Left err

-- |Connect to a server with TLS.
connectWithTLS :: MonadIO m => HostName -> Int -> m (Either String ConnectionConfig)
connectWithTLS host port = connectWithTLS' host port N.defaultCiphers

-- |Connect to a server without TLS, supplying your own list of
-- ciphers, ordered by preference.
connectWithTLS' :: MonadIO m => HostName -> Int -> [Cipher] -> m (Either String ConnectionConfig)
connectWithTLS' host port ciphers = do
  res <- N.connectWithTLS' host port ciphers

  return $
    case res of
      Right (sock, ctx) -> Right ConnectionConfig
                            { _socket = sock
                            , _tls    = Just ctx
                            , _server = host
                            , _port   = port
                            }
      Left err -> Left err

-- *Event loop

-- |Run the event loop for a server, receiving messages and handing
-- them off to handlers as appropriate.
run :: MonadIO m => ConnectionConfig -> InstanceConfig -> m ()
run cconf iconf = liftIO . void . flip runStateT iconf $ runReaderT runner cconf

-- |The event loop.
runner :: IRC ()
runner = do
  -- Set the nick and username
  theNick <- _nick     <$> instanceConfig
  theUser <- _username <$> instanceConfig
  theReal <- _realname <$> instanceConfig

  send $ user theUser theReal
  send $ nick theNick

  -- Connect to channels
  mapM_ (send . join) . _channels <$> instanceConfig

  -- Event loop
  forever $ do
    msg <- N.recv
    case msg of
      Just msg' -> do
        logmsg True msg'

        event <- toEvent msg' send

        handlers <- getHandlersFor event . _eventHandlers <$> instanceConfig
        -- TODO: Parallelise this (requires bunging state behind an MVar)
        mapM_ ($ event) handlers

      -- Ignore malformed messages
      Nothing   -> return ()

-- |Get the event handlers for an event.
getHandlersFor :: Event -> [EventHandler] -> [Event -> IRC ()]
getHandlersFor e ehs = [_eventFunc eh | eh <- ehs, _matchType eh `elem` [EEverything, _eventType e]]

-- |Log a message to stdout and the internal log
logmsg :: Bool -> Message -> IRC ()
logmsg fromsrv msg = do
  now <- liftIO getCurrentTime

  liftIO . putStrLn $ unwords [ formatTime defaultTimeLocale "%c" now
                              , if fromsrv then "<---" else "--->"
                              , unpack . decodeUtf8 . encode $ msg
                              ]

-- *Messaging

-- |Send a message, using TLS if enabled.
send :: Message -> IRC ()
send msg = do
  -- Block until the flood delay passes
  now     <- liftIO getCurrentTime
  lastMsg <- _lastMessageTime <$> instanceConfig
  flood   <- fromIntegral . _floodDelay <$> instanceConfig

  let nextMsg = addUTCTime flood lastMsg
  when (nextMsg > now) $
    -- threadDelay uses microseconds, NominalDiffTime is in seconds,
    -- but with a precision of nanoseconds.
    liftIO . threadDelay . ceiling $ 1000000 * diffUTCTime nextMsg now

  -- Update the last message time
  ic   <- instanceConfig
  now' <- liftIO getCurrentTime
  putInstanceConfig ic { _lastMessageTime = now' }

  -- Send the message
  logmsg False msg
  N.send msg

-- *Disconnecting

-- |Disconnect from a server, properly tearing down the TLS session
-- (if there is one).
disconnect :: IRC ()
disconnect = N.disconnect

-- *Default configuration

-- |Construct a default IRC configuration from a nick
defaultIRCConf :: Text -> InstanceConfig
defaultIRCConf n = InstanceConfig
                     { _nick          = n
                     , _username      = n
                     , _realname      = n
                     , _channels      = []
                     , _ctcpVer       = "idte-0.0.0.1"
                     , _floodDelay    = 1
                     , _lastMessageTime = UTCTime (fromGregorian 0 0 0) 0
                     , _eventHandlers = [ EventHandler "Respond to server PING requests"  EPing pingHandler
                                        , EventHandler "Respond to CTCP PING requests"    ECTCP ctcpPingHandler
                                        , EventHandler "Respond to CTCP VERSION requests" ECTCP ctcpVersionHandler
                                        , EventHandler "Respond to CTCP TIME requests"    ECTCP ctcpTimeHandler
                                        , EventHandler "Mangle the nick on collision"     ENumeric nickMangler
                                        ]
                     }

-- |Respond to pings
pingHandler :: Event -> IRC ()
pingHandler ev =
  case _message ev of
    Ping target -> send $ pong target
    _ -> return ()

-- |Respond to CTCP PINGs
ctcpPingHandler :: Event -> IRC ()
ctcpPingHandler ev =
  case (_source ev, _message ev) of
    (User n, CTCP p xs) | toUpper p == "PING" -> send $ ctcpReply n "PING" xs
    _ -> return ()

-- |Respond to CTCP VERSIONs
ctcpVersionHandler :: Event -> IRC ()
ctcpVersionHandler ev = do
  ver <- _ctcpVer <$> instanceConfig
  case (_source ev, _message ev) of
    (User n, CTCP v []) | toUpper v == "VERSION" -> send $ ctcpReply n "VERSION" [ver]
    _ -> return ()

-- |Respond to CTCP TIMEs
ctcpTimeHandler :: Event -> IRC ()
ctcpTimeHandler ev = do
  now <- liftIO getCurrentTime
  case (_source ev, _message ev) of
    (User n, CTCP t []) | toUpper t == "TIME" -> send $ ctcpReply n "TIME" [T.pack $ formatTime defaultTimeLocale "%c" now]
    _ -> return ()

-- |Mangle the nick if there's a collision when we set it
nickMangler :: Event -> IRC ()
nickMangler ev = do
  theNick <- _nick <$> instanceConfig

  case _message ev of
    -- ERR_ERRONEUSNICKNAME: Bad characters in nick
    Numeric 432 _ -> setNick $ fresh theNick
    -- ERR_NICKNAMEINUSE: Nick in use
    Numeric 433 _ -> setNick $ mangle theNick
    -- ERR_NICKCOLLISION: Nick registered
    Numeric 436 _ -> setNick $ mangle theNick
    _ -> return ()

  where fresh n  = takeEnd nicklen $ let n' = T.filter isAlphaNum n
                                     in if T.length n' == 0
                                        then "f"
                                        else n'

        mangle n = takeEnd nicklen $ (n <> "1") `fromMaybe` charsubst n

        -- Maximum length of a nick
        nicklen = 16

        -- List of substring substitutions. It's important that
        -- these don't contain any loops!
        charsubst = transform [ ("i", "1")
                              , ("I", "1")
                              , ("l", "1")
                              , ("L", "1")
                              , ("o", "0")
                              , ("O", "0")
                              , ("A", "4")
                              , ("0", "1")
                              , ("1", "2")
                              , ("2", "3")
                              , ("3", "4")
                              , ("4", "5")
                              , ("5", "6")
                              , ("6", "7")
                              , ("7", "8")
                              , ("8", "9")
                              , ("9", "-")
                              ]

        -- Attempt to transform some text by the substitutions.
        transform ((from, to):trs) txt = case breakOn' from txt of
                                           Just (before, after) -> Just $ before <> to <> after
                                           Nothing -> transform trs txt
        transform [] _ = Nothing

        breakOn' delim txt = let (before, after) = breakOn delim txt
                             in if T.length after >= T.length delim
                                then Just (before, T.drop (T.length delim) after)
                                else Nothing

-- *Utilities

-- |Update the nick in the instance configuration and also send an
-- update message to the server.
setNick :: Text -> IRC ()
setNick new = do
  iconf <- instanceConfig
  putInstanceConfig iconf { _nick = new }
  send $ nick new
