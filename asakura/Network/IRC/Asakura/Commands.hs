{-# LANGUAGE OverloadedStrings #-}

-- |An abstraction over named event handlers which take an argument
-- list.
module Network.IRC.Asakura.Commands
    ( -- *State
      CommandState
    , initialise
    -- *Events
    , eventRunner
    -- *Registering commands
    , registerCommand
    -- *Miscellaneous
    , setPrefix
    , setChannelPrefix
    ) where

import Control.Concurrent.STM     (TVar, atomically, newTVar, readTVar, writeTVar)
import Control.Monad.IO.Class     (MonadIO, liftIO)
import Data.Maybe                 (fromMaybe)
import Data.Monoid                ((<>))
import Data.Text                  (Text, isPrefixOf, splitOn)
import Network                    (HostName)
import Network.IRC.Asakura.Events (runAlways, runEverywhere)
import Network.IRC.Asakura.Types
import Network.IRC.IDTE.Types     ( ConnectionConfig(..)
                                  , Event(..), EventType(..)
                                  , InstanceConfig(..), IRC, IrcMessage(..), IRCState
                                  , Source(..)
                                  , getConnectionConfig, getInstanceConfig)

import qualified Data.Text as T

-- *State

-- |The private state of this module, used by functions to access the
-- state.
--
-- TODO: Per-channel command prefix.
data CommandState = CommandState
    { _commandPrefix :: TVar Text
    -- ^ A substring which must, if the bot was not addressed
    -- directly, preceed the command name in order for it to be a
    -- match.
    , _channelPrefixes :: TVar [((HostName, Text), Text)]
    -- ^Channel-specific command prefixes, which will be used instead
    -- of the generic prefix if present.
    , _commandList   :: TVar [(Text, [Text] -> IRCState -> Event -> Bot (IRC ()))] }

-- |Initialise the state for this module. This should only be done
-- once.
initialise :: MonadIO m
           => Text
           -- ^The command prefix (may later be changed).
           -> m CommandState
initialise pref = do
  tvarP  <- liftIO . atomically . newTVar $ pref
  tvarCP <- liftIO . atomically . newTVar $ []
  tvarL  <- liftIO . atomically . newTVar $ []
  return CommandState { _commandPrefix   = tvarP
                      , _channelPrefixes = tvarCP
                      , _commandList     = tvarL }

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

-- |Check if a PRIVMSG is calling a known command and, if so, run it.
--
-- TODO: Integrate this with permissions (todo: permissions)
runCmd :: CommandState -> IRCState -> Event -> Bot (IRC ())
runCmd state ircstate ev = do
  let Privmsg msg = _message ev

  -- Extract the channel name, if there is one, so we can use the
  -- channel-specific prefix.
  let host = _server $ getConnectionConfig ircstate
  let chan = case _source ev of
               Channel _ c -> Just c
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

  -- Split off the prefix
  let splitted = case _source ev of
                   Channel _ _ -> mapFirst (stripPrefix msg) $ prefixes prefix nick

                   -- The empty string is a valid prefix in queries.
                   User _      -> mapFirst (stripPrefix msg) $ prefixes prefix nick ++ [""]

                   _           -> Nothing

  -- Try to find a matching command
  case splitted of
    Just (cmd, args) -> case lookup cmd commands of
                         Just handler -> handler args ircstate ev
                         Nothing      -> return $ return ()
    Nothing -> return $ return ()

  where -- Map a Maybe-producing function over a list, returning the
        -- first Just.
        mapFirst f (x:xs) = case f x of
                              Just x' -> Just x'
                              Nothing -> mapFirst f xs
        mapFirst _ []     = Nothing

        -- Check if the command has a given prefix and, if so, strip
        -- it off. If the prefix *is* the command, the next argument
        -- (if present) is the actual command.
        stripPrefix msg prefix | prefix `isPrefixOf` msg = Just . splitCmd . T.strip . T.drop (T.length prefix) $ msg
                               | otherwise = Nothing

        -- Split a trimmed message into (command, args)
        splitCmd msg = case splitOn " " msg of
                         (cmd:args) -> (cmd, args)
                         _          -> (msg, [])

        -- List of accepted command prefixes.
        prefixes prefix nick = [ nick <> ":"
                               , nick <> ","
                               , "@" <> nick
                               , nick
                               , prefix
                               ]

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
                -> ([Text] -> IRCState -> Event -> Bot (IRC ()))
                -- ^The command handler
                -> m ()
registerCommand state cmd f = liftIO . atomically $ do
  let tvarL = _commandList state

  commands <- readTVar tvarL
  writeTVar tvarL $ (cmd, f) : commands

-- *Miscellaneous

-- |Change the command prefix.
setPrefix :: MonadIO m => CommandState -> Text -> m ()
setPrefix state prefix = liftIO . atomically $ writeTVar (_commandPrefix state) prefix

-- |Change the command prefix for a specific channel.
setChannelPrefix :: MonadIO m => CommandState -> HostName -> Text -> Text -> m ()
setChannelPrefix state network channel prefix = liftIO . atomically $ do
  let tvarCP = _channelPrefixes state

  prefixes <- readTVar tvarCP
  writeTVar tvarCP $ ((network, channel), prefix) : filter ((/=(network, channel)) . fst) prefixes
