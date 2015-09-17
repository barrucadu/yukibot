{-# LANGUAGE OverloadedStrings #-}

-- |An abstraction over named event handlers which take an argument
-- list.
module Network.IRC.Asakura.Commands
    ( -- *State
      CommandState
    , CommandStateSnapshot(..)
    , CommandDef(..)
    , defaultCommandState
    -- *Events
    , eventRunner
    -- *Registering commands
    , registerCommand
    -- *Miscellaneous
    , setPrefix
    , setChannelPrefix
    , unsetChannelPrefix
    ) where

import Control.Applicative        ((<$>))
import Control.Concurrent.STM     (atomically, readTVar, writeTVar)
import Control.Monad.IO.Class     (MonadIO, liftIO)
import Data.ByteString            (ByteString)
import Data.Maybe                 (fromMaybe, mapMaybe)
import Data.Monoid                ((<>))
import Data.List                  (stripPrefix)
import Data.Text                  (Text, isPrefixOf, splitOn)
import Network.IRC.Asakura.Commands.State
import Network.IRC.Asakura.Events (runAlways, runEverywhere)
import Network.IRC.Asakura.Types
import Network.IRC.Client         (send)
import Network.IRC.Client.Types   ( ConnectionConfig(..)
                                  , Event(..), UnicodeEvent, EventType(..)
                                  , InstanceConfig(..), IRC, IRCState
                                  , Message(..), Source(..)
                                  , getConnectionConfig, getInstanceConfig)

import qualified Data.Text as T

-- *Events

-- |Construct an event handler which will run commands registered in
-- this state.
eventRunner :: CommandState -> AsakuraEventHandler
eventRunner state = AsakuraEventHandler
                      { _description = "Run named commands from PRIVMSGs"
                      , _matchType   = EPrivmsg
                      , _eventFunc   = runCmd state
                      , _appliesTo   = runEverywhere
                      , _appliesDef  = runAlways
                      }

-- |Check if a PRIVMSG is calling a known command and, if so, run it i
-- the user has appropriate permissions.
runCmd :: CommandState -> IRCState -> UnicodeEvent -> Bot (IRC ())
runCmd state ircstate ev = do
  -- Extract the channel name, if there is one, so we can use the
  -- channel-specific prefix.
  let host = _server $ getConnectionConfig ircstate
  let chan = case _source ev of
               Channel c _ -> Just c
               _           -> Nothing

  -- Read the state
  (nick, prefix, commands) <- liftIO . atomically $ do
    iconf        <- readTVar . getInstanceConfig $ ircstate
    defprefix    <- readTVar . _commandPrefix    $ state
    chanprefixes <- readTVar . _channelPrefixes  $ state
    commands     <- readTVar . _commandList      $ state

    -- Look for a channel-specific prefix
    let pref = case chan of
                 Just c  -> defprefix `fromMaybe` lookup (host, c) chanprefixes
                 Nothing -> defprefix

    return (_nick iconf, pref, commands)

  -- Try to find a matching command
  case splitCommand nick ev prefix of
    Just bits -> case findCommand bits commands of
                  [(_, args, cdef)] -> _action cdef args ircstate ev
                  [] -> return $ return ()
                  _ -> return $ ambiguous ev

    Nothing -> return $ return ()

-- |Strip a command prefix and split it up into parts
splitCommand :: Text -> UnicodeEvent -> Text -> Maybe [Text]
splitCommand nick ev prefix = case _source ev of
                                Channel _ _ -> mapFirst stripPrefix $ prefixes prefix nick

                              -- The empty string is a valid prefix in queries.
                                User _      -> mapFirst stripPrefix $ prefixes prefix nick ++ [""]

                                _  -> Nothing

    where Privmsg _ (Right msg) = _message ev

          -- Map a Maybe-producing function over a list, returning the
          -- first Just.
          mapFirst f (x:xs) = case f x of
                                Just x' -> Just x'
                                Nothing -> mapFirst f xs
          mapFirst _ []     = Nothing

          -- Check if the command has a given prefix and, if so, strip
          -- it off.
          stripPrefix pref | pref `isPrefixOf` msg = Just . splitOn " " . T.strip . T.drop (T.length pref) $ msg
                           | otherwise = Nothing

          -- List of accepted command prefixes.
          prefixes pref nck = [ nck <> ":"
                              , nck <> ","
                              , "@" <> nck
                              , nck
                              , pref
                              ]

-- |Find all commands which could match this instruction.
findCommand :: [Text] -> [([Text], CommandDef)] -> [([Text], [Text], CommandDef)]
findCommand msg = mapMaybe matchCmd
    where matchCmd (cmd, cdef) = flip ((,,) cmd) cdef <$> stripPrefix cmd msg

-- |Complain about a command being ambigious.
--
-- TODO: Make the response configurable.
ambiguous :: UnicodeEvent -> IRC ()
ambiguous ev = case _source ev of
                 Channel c _ -> send . Privmsg c . Right $ "Ambiguous command: tell off my master."
                 User    n   -> send . Privmsg n . Right $ "Ambiguous command: tell off my master."
                 _ -> return ()

-- *Registering commands

-- |Register a new command.
--
-- Commands are like event handlers, but also take the list of
-- space-delimited arguments to the command as a first argument, which
-- saves the common case of needing to do so manually in the command
-- body.
registerCommand :: MonadIO m
                => CommandState
                -- ^The initialised state
                -> CommandDef
                -- ^The command handler
                -> m ()
registerCommand state cdef = liftIO . atomically $ do
  let tvarL = _commandList state

  commands <- readTVar tvarL
  writeTVar tvarL $ (_verb cdef, cdef) : commands

-- *Miscellaneous

-- |Change the command prefix.
setPrefix :: MonadIO m => CommandState -> Text -> m ()
setPrefix state prefix = liftIO . atomically $ writeTVar (_commandPrefix state) prefix

-- |Change the command prefix for a specific channel.
setChannelPrefix :: MonadIO m => CommandState -> ByteString -> Text -> Text -> m ()
setChannelPrefix state network channel prefix = liftIO . atomically $ do
  let tvarCP = _channelPrefixes state

  prefixes <- readTVar tvarCP
  writeTVar tvarCP $ ((network, channel), prefix) : filter ((/=(network, channel)) . fst) prefixes

-- |Remove the command prefix for a specific channel.
unsetChannelPrefix :: MonadIO m => CommandState -> ByteString -> Text -> m ()
unsetChannelPrefix state network channel = liftIO . atomically $ do
  let tvarCP = _channelPrefixes state

  prefixes <- readTVar tvarCP
  writeTVar tvarCP $ filter ((/=(network, channel)) . fst) prefixes
