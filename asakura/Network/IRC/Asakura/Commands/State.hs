{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

-- |Internal state for the command runner module.
module Network.IRC.Asakura.Commands.State where

import Control.Applicative             ((<$>), (<*>))
import Control.Concurrent.STM          (TVar, newTVar, readTVar)
import Data.Aeson                      (FromJSON(..), ToJSON(..), Value(..), (.=), (.:), (.:?), (.!=), object)
import Data.Default.Class              (Default(..))
import Data.Map                        (Map)
import Data.Text                       (Text)
import Network                         (HostName)
import Network.IRC.IDTE                (Event, IRC, IRCState)
import Network.IRC.Asakura.Permissions (PermissionLevel, PermissionState)
import Network.IRC.Asakura.State
import Network.IRC.Asakura.Utils       (collect)
import Network.IRC.Asakura.Types       (Bot)

import qualified Data.Map as M

-- *State

-- |The private state of this module, used by functions to access the
-- state.
data CommandState = CommandState
    { _commandPrefix   :: TVar Text
    -- ^ A substring which must, if the bot was not addressed
    -- directly, preceed the command name in order for it to be a
    -- match.
    , _channelPrefixes :: TVar [((HostName, Text), Text)]
    -- ^Channel-specific command prefixes, which will be used instead
    -- of the generic prefix if present.
    , _commandList     :: TVar [(Text, CommandDef)]
    -- ^List of commands
    , _pstate          :: PermissionState
    -- ^State of the permission system.
    }

-- |A single command.
data CommandDef = CommandDef
    { _permission :: Maybe PermissionLevel
    -- ^The minimum required permission level, if set
    , _action     :: [Text] -> IRCState -> Event -> Bot (IRC ())
    -- ^The function to run on a match. This is like a regular event
    -- handler, except it takes the space-separated list of arguments
    -- to the command as the first parameter.
    }

-- *Snapshotting

-- |A snapshot of the private command state, containing all the
-- prefixes.
data CommandStateSnapshot = CommandStateSnapshot
    { _ssDefPrefix    :: Text
    , _ssChanPrefixes :: Map HostName (Map Text Text)
    }

instance Default CommandStateSnapshot where
    -- |Prefix of "!", no channel prefixes.
    def = CommandStateSnapshot
            { _ssDefPrefix    = "!"
            , _ssChanPrefixes = M.empty
            }

instance ToJSON CommandStateSnapshot where
    toJSON ss | M.null (_ssChanPrefixes ss) = object [ "defaultPrefix"   .= _ssDefPrefix ss ]
              | otherwise = object [ "defaultPrefix"   .= _ssDefPrefix ss
                                   , "channelPrefixes" .= (toJSON . _ssChanPrefixes $ ss)
                                   ]

instance FromJSON CommandStateSnapshot where
    parseJSON (Object v) = CommandStateSnapshot
                             <$> v .: "defaultPrefix"
                             <*> v .:? "channelPrefixes" .!= M.fromList []
    parseJSON _ = fail "Bad type"

instance Snapshot CommandState CommandStateSnapshot where
    snapshotSTM state = do
      defPrefix    <- readTVar . _commandPrefix   $ state
      chanPrefixes <- readTVar . _channelPrefixes $ state

      return CommandStateSnapshot { _ssDefPrefix    = defPrefix
                                  , _ssChanPrefixes = toPrefixTree chanPrefixes
                                  }

      where toPrefixTree = fmap M.fromList . M.fromList . collect . map flipTuple

            flipTuple ((host, chan), pref) = (host, (chan, pref))

instance Rollback CommandStateSnapshot (PermissionState -> CommandState) where
    rollbackSTM ss = do
      tvarP  <- newTVar . _ssDefPrefix $ ss
      tvarCP <- newTVar . fromPrefixTree . _ssChanPrefixes $ ss
      tvarL  <- newTVar []

      return $ \pstate -> CommandState { _commandPrefix   = tvarP
                                      , _channelPrefixes = tvarCP
                                      , _commandList     = tvarL
                                      , _pstate          = pstate
                                      }
      where fromPrefixTree = concatMap fromNets . M.toList . fmap M.toList

            fromNets (host, chans) = map (fromChans host) chans

            fromChans host (chan, pref) = ((host, chan), pref)
