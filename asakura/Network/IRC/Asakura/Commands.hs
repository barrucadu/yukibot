{-# LANGUAGE OverloadedStrings #-}

-- |An abstraction over named event handlers which take an argument
-- list.
module Network.IRC.Asakura.Commands
    ( -- *State
      CommandState
    , CommandStateSnapshot(..)
    , CommandDef(..)
    -- *Events
    , eventRunner
    -- *Registering commands
    , registerCommand
    , registerCommand'
    , registerLongCommand
    , registerLongCommand'
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
import Network.IRC.Asakura.Permissions (PermissionLevel, PermissionState, hasPermission)
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
                  [(_, args, cdef)] -> do
                    -- Check the permissions, and don't run the
                    -- command if the user isn't allowed.
                    allowed <- case _source ev of
                                Channel _ n -> isAllowed cdef n (_pstate state) host chan
                                User    n   -> isAllowed cdef n (_pstate state) host chan
                                _ -> return False

                    if allowed
                    then _action cdef args ircstate ev
                    else return $ berate ev

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

-- |Check if a user is allowed to run a command
isAllowed :: MonadIO m => CommandDef -> Text -> PermissionState -> ByteString -> Maybe Text -> m Bool
isAllowed cdef nick pstate host chan = case _permission cdef of
                                         Just req -> hasPermission pstate nick host chan req
                                         Nothing  -> return True

-- |Tell a user off for not having permissions.
--
-- TODO: Make the response configurable.
berate :: UnicodeEvent -> IRC ()
berate ev = case _source ev of
              Channel c n -> send . Privmsg c . Right $ "I'm sorry " <> n <> ", I'm afraid I can't do that."
              User    n   -> send . Privmsg n . Right $ "I'm sorry " <> n <> ", I'm afraid I can't do that."
              _ -> return ()

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
                -> Text
                -- ^The command name
                -> Maybe PermissionLevel
                -- ^The minimum required permission level
                -> ([Text] -> IRCState -> UnicodeEvent -> Bot (IRC ()))
                -- ^The command handler
                -> m ()
registerCommand state cmd = registerLongCommand state [cmd]

-- |Register a 'CommandDef' as a command.
registerCommand' :: MonadIO m => CommandState -> Text -> CommandDef -> m ()
registerCommand' state cmd = registerLongCommand' state [cmd]

-- |Register a multi-word command.
registerLongCommand :: MonadIO m => CommandState -> [Text] -> Maybe PermissionLevel -> ([Text] -> IRCState -> UnicodeEvent -> Bot (IRC ())) -> m ()
registerLongCommand state cmd perm f = registerLongCommand' state cmd CommandDef
                                         { _permission = perm
                                         , _action     = f
                                         }

-- |Register a 'CommandDef' as a multi-word command.
registerLongCommand' :: MonadIO m => CommandState -> [Text] -> CommandDef -> m ()
registerLongCommand' state cmd cdef = liftIO . atomically $ do
  let tvarL = _commandList state

  commands <- readTVar tvarL
  writeTVar tvarL $ (cmd, cdef) : commands

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
