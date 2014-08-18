{-# LANGUAGE OverloadedStrings #-}

-- |Types for messages and their handlers.
module Network.IRC.IDTE.Events
    ( Event(..)
    , Source(..)
    , IrcMessage(..)
    , ModeChange(..)
    , toEvent
    , decode
    , encode
    ) where

import Control.Applicative ((<$>))
import Data.ByteString     (ByteString)
import Data.Char           (isDigit)
import Data.Monoid         ((<>))
import Data.String         (fromString)
import Data.Text           (Text, unpack, pack, singleton)
import Data.Text.Encoding  (decodeUtf8, encodeUtf8)
import Network.IRC         (Message(..), Prefix(..))
import Network.IRC.IDTE.Client
import Network.IRC.IDTE.CTCP
import Network.IRC.IDTE.Utils

import qualified Data.Text   as T
import qualified Network.IRC as I

-- *Types

-- |An event has a message, some information on the source, and a
-- reply function.
data Event = Event
    { _rawMessage :: Message
    -- ^The original message, split into parts and nothing more.
    , _source     :: Source
    -- ^The source of the message.
    , _message    :: IrcMessage
    -- ^The message data, split into a sum type.
    , _reply      :: IrcMessage -> IRC ()
    -- ^Sends a message to the source of this event.
    , _send       :: Source -> IrcMessage -> IRC ()
    -- ^Send a message
    , _sendRaw    :: Message -> IRC ()
    -- ^Send a raw message
    }

-- |The source of a message.
data Source = Server
            -- ^The message comes from the server.
            | Channel Text Text
            -- ^A channel the client is in. The first Text is the
            -- nick, the second, the channel name.
            | User Text
            -- ^A query from a user.
            | UnknownSource
            -- ^The source could not be determined, see the raw
            -- message

-- |A decoded message
data IrcMessage = Privmsg Text
                -- ^The client has received a message, which may be to
                -- a channel it's in.
                --
                -- CTCPs will, however, not raise this event (despite
                -- being sent as a PRIVMSG).

                | Notice Text
                -- ^Like a PRIVMSG, except an automatic reply must
                -- *not* be generated.

                | CTCP Text [Text]
                -- ^A CTCP has been received.

                | Nick Text
                -- ^Someone has updated their nick. The given nickname
                -- is the new one.

                | Join Text
                -- ^Someone has joined a channel the client is in.

                | Part Text (Maybe Text)
                -- ^Someone has left a channel the client is in.

                | Quit Text (Maybe Text)
                -- ^Someone has quit a channel the client is in.

                | Mode [ModeChange]
                -- ^Some mode changes have been applied to a channel
                -- the client is in, or a user in a channel the client
                -- is in.

                | Topic Text
                -- ^The topic of a channel the client is in has been
                -- updated.

                | Invite Text Text
                -- ^The client has been invited to a channel. The
                -- first text is the nick of the inviter.

                | Kick Text (Maybe Text)
                -- ^Someone has been kicked from a channel the client
                -- is in.

                | Ping Text
                -- ^A ping has been received (probably from the
                -- server, due to a period of inactivity). The Text is
                -- where the PONG should be sent.

                | Numeric Int [Text]
                -- ^One of the many numeric codes has been received in
                -- response to something the client did.

                | UnknownMessage
                -- ^The message could not be decoded, see the raw
                -- message.

-- |A single mode change to a channel or user.
data ModeChange = ModeChange
    { _set      :: Bool
    -- ^Whether the mode has been enabled or disabled
    , _flag     :: Text
    -- ^The mode name
    , _modeargs :: Maybe [Text]
    -- ^Any arguments to the mode change
    }

-- *Decoding messages

-- |Turn a message and a message sending function into an event.
toEvent :: Message
        -- ^The message to decode
        -> (Message -> IRC ())
        -- ^Message sending function
        -> IRC Event
toEvent msg send = do
  nick <- _nick <$> instanceConfig

  let (source, message) = decode nick msg

  return Event { _rawMessage = msg
               , _source     = source
               , _message    = message
               , _reply      = \m -> case encode source m of
                                      Just m' -> send m'
                                      Nothing -> return ()
               , _send       = \s m -> case encode s m of
                                        Just m' -> send m'
                                        Nothing -> return ()
               , _sendRaw    = send
               }

-- |Decode a message into a source and (nice) message, or die (return
-- a silly value) trying.
--
-- See http://tools.ietf.org/html/rfc2812
decode :: Text
       -- ^The nick of the client (used for disambiguationg
       -- channels/nicks)
       -> Message -> (Source, IrcMessage)
decode nick msg = case msg of
                    Message (Just (NickName n _ _)) "PRIVMSG" [t, m] | t == nick' -> (user n,   privmsg `orCTCP` ctcp $ m)
                                                                     | otherwise -> (chan n t, privmsg `orCTCP` ctcp $ m)
                    Message (Just (NickName n _ _)) "NOTICE"  [t, m] | t == nick' -> (user n,   notice  `orCTCP` ctcp $ m)
                                                                     | otherwise -> (chan n t, notice  `orCTCP` ctcp $ m)

                    Message (Just (NickName n _ _)) "NICK"   [n']      -> (user n,   Nick   <$ n')
                    Message (Just (NickName n _ _)) "JOIN"   [c]       -> (chan n c, Join   <$ c)
                    Message (Just (NickName n _ _)) "PART"   [c]       -> (chan n c, Part   <$ c <$: Nothing)
                    Message (Just (NickName n _ _)) "PART"   [c, r]    -> (chan n c, Part   <$ c <$: Just r)
                    Message (Just (NickName n _ _)) "QUIT"   []        -> (user n,   Quit   <$ n <$: Nothing)
                    Message (Just (NickName n _ _)) "QUIT"   [r]       -> (user n,   Quit   <$ n <$: Just r)
                    Message (Just (NickName n _ _)) "KICK"   [c, u]    -> (chan n c, Kick   <$ u <$: Nothing)
                    Message (Just (NickName n _ _)) "KICK"   [c, u, r] -> (chan n c, Kick   <$ u <$: Just r)
                    Message (Just (NickName n _ _)) "INVITE" [_, c]    -> (user n,   Invite <$ n <$ c)
                    Message (Just (NickName n _ _)) "TOPIC"  [c, t]    -> (chan n c, Topic  <$ t)

                    Message (Just (NickName n _ _)) "MODE" (t:ms) | n == t     -> (user n,   Mode $ toModeChanges ms)
                                                                  | otherwise -> (chan n t, Mode $ toModeChanges ms)

                    Message (Just (I.Server _))     "PING" [s1]    -> (server, Ping <$ s1)
                    Message (Just (I.Server _))     "PING" [_, s2] -> (server, Ping <$ s2)
                    Message Nothing                 "PING" [s1]    -> (server, Ping <$ s1)
                    Message Nothing                 "PING" [_, s2] -> (server, Ping <$ s2)
                    Message (Just (NickName n _ _)) "PING" [s1]    -> (user n, Ping <$ s1)
                    Message (Just (NickName n _ _)) "PING" [_, s2] -> (user n, Ping <$ s2)

                    Message (Just (I.Server _)) n args | isNumeric n -> (server, Numeric (read . unpack <$ n) <$: args)
                    Message Nothing             n args | isNumeric n -> (server, Numeric (read . unpack <$ n) <$: args)

                    _ -> (UnknownSource, UnknownMessage)

    where nick' = encodeUtf8 nick

          server   = Network.IRC.IDTE.Events.Server
          user n   = User <$ n
          chan n c = Channel <$ n <$ c

          privmsg bs = Privmsg <$ bs
          notice bs  = Notice  <$ bs
          ctcp bs    = uncurry CTCP $ fromCTCP bs

          isNumeric =  T.all isDigit . decodeUtf8

-- |Convert a list of textual mode changes to ModeChanges.
toModeChanges :: [ByteString] -> [ModeChange]
toModeChanges ms = modeChange $ map (unpack . decodeUtf8) ms
    where modeChange ((p : f : []) : ms) = let (args, rest) = collectArgs ms
                                           in mchange p f (Just args) : modeChange rest

          modeChange ((p : f : fs) : ms) = mchange p f Nothing : modeChange ((p : fs) : ms)

          modeChange (_ : ms) = modeChange ms
          modeChange [] = []

          mchange p f args = ModeChange (p == '+') (singleton f) $ fmap (map pack) args

          collectArgs = break (\(p:_) -> p == '+' || p == '-')

-- *Encoding messages

-- |Encode a target (source) and message into something we can send
-- down the wire.
--
-- Decoding followed by encoding always results in a Just, if you do
-- weird things with your messages this may well result in a Nothing,
-- in which case you can construct it yourself.
encode :: Source -> IrcMessage -> Maybe Message
encode (Channel _ c) (Privmsg m) = Just $ mkMessage "PRIVMSG" >$: [c, m]
encode (User n)      (Privmsg m) = Just $ mkMessage "PRIVMSG" >$: [n, m]
encode (Channel _ c) (Notice m)  = Just $ mkMessage "NOTICE"  >$: [c, m]
encode (User n)      (Notice m)  = Just $ mkMessage "NOTICE"  >$: [n, m]
encode (Channel _ c) (CTCP v xs) = Just $ mkMessage "PRIVMSG" [encodeUtf8 c, toByteString $ toCTCP v xs]
encode (User n)      (CTCP v xs) = Just $ mkMessage "PRIVMSG" [encodeUtf8 n, toByteString $ toCTCP v xs]
encode _ (Nick n)                = Just $ mkMessage "NICK" >$: [n]
encode _ (Join c)                = Just $ mkMessage "JOIN" >$: [c]
encode _ (Part c (Just r))       = Just $ mkMessage "PART" >$: [c, r]
encode _ (Part c Nothing)        = Just $ mkMessage "PART" >$: [c]
encode _ (Quit _ (Just r))       = Just $ mkMessage "QUIT" >$: [r]
encode _ (Quit _ Nothing)        = Just $ mkMessage "QUIT" []
encode (User n) (Mode ms)        = Just $ mkMessage "MODE" >$: (n : toModeStrs ms)
encode (Channel _ c) (Mode ms)   = Just $ mkMessage "MODE" >$: (c : toModeStrs ms)
encode (Channel _ c) (Topic t)   = Just $ mkMessage "TOPIC" >$: [c, t]
encode _ (Invite n c)            = Just $ mkMessage "INVITE" >$: [n, c]
encode (Channel _ c) (Kick n (Just r)) = Just $ mkMessage "KICK" >$: [c, n, r]
encode (Channel _ c) (Kick n Nothing)  = Just $ mkMessage "KICK" >$: [c, n]
encode _ (Ping s1)               = Just $ mkMessage "PING" >$: [s1]
encode _ (Numeric n xs)          = Just $ mkMessage (fromString $ show n) >$: xs
encode _ _ = Nothing

-- |Build a message from parts
mkMessage :: ByteString -> [ByteString] -> Message
mkMessage = Message Nothing

-- |Decode a list of mode changes into a command
toModeStrs :: [ModeChange] -> [Text]
toModeStrs = concatMap modeChange
    where modeChange (ModeChange True  f (Just args)) = ("+" <> f) : args
          modeChange (ModeChange True  f Nothing)     = ["+" <> f]
          modeChange (ModeChange False f (Just args)) = ("-" <> f) : args
          modeChange (ModeChange False f Nothing)     = ["-" <> f]
