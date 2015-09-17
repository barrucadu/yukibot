{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

-- |Internal state for the command runner module.
module Network.IRC.Asakura.Commands.State where

import Control.Applicative       ((<$>), (<*>))
import Control.Concurrent.STM    (TVar, newTVar, readTVar)
import Data.Aeson                (FromJSON(..), ToJSON(..), Value(..), (.=), (.:?), (.!=), object)
import Data.ByteString           (ByteString)
import Data.ByteString.Char8     (pack, unpack)
import Data.Map                  (Map)
import Data.Text                 (Text)
import Network.IRC.Client        (UnicodeEvent, IRC, IRCState)
import Network.IRC.Asakura.State
import Network.IRC.Asakura.Utils (collect)
import Network.IRC.Asakura.Types (Bot)

import qualified Data.Map as M

-- *State

-- |The private state of this module, used by functions to access the
-- state.
data CommandState = CommandState
    { _commandPrefix   :: TVar Text
    -- ^ A substring which must, if the bot was not addressed
    -- directly, preceed the command name in order for it to be a
    -- match.
    , _channelPrefixes :: TVar [((ByteString, Text), Text)]
    -- ^Channel-specific command prefixes, which will be used instead
    -- of the generic prefix if present.
    , _commandList     :: TVar [([Text], CommandDef)]
    -- ^List of commands
    }

-- |A single command.
data CommandDef = CommandDef
    { _verb   :: [Text]
    -- ^The name of the command, this is what comes between the prefix
    -- and the arguments.
    , _help :: Text
    -- ^Help text for the command.
    , _action :: [Text] -> IRCState -> UnicodeEvent -> Bot (IRC ())
    -- ^The function to run on a match. This is like a regular event
    -- handler, except it takes the space-separated list of arguments
    -- to the command as the first parameter.
    }

-- *Snapshotting

-- |A snapshot of the private command state, containing all the
-- prefixes.
data CommandStateSnapshot = CSS
    { _ssDefPrefix    :: Text
    , _ssChanPrefixes :: Map String (Map Text Text)
    }

-- |Prefix of "!", no channel prefixes.
defaultCommandState :: CommandStateSnapshot
defaultCommandState = CSS
  { _ssDefPrefix    = "!"
  , _ssChanPrefixes = M.empty
  }

instance ToJSON CommandStateSnapshot where
    toJSON ss | M.null (_ssChanPrefixes ss) = object [ "defaultPrefix"  .= _ssDefPrefix ss ]
              | otherwise = object [ "defaultPrefix"   .= _ssDefPrefix ss
                                   , "channelPrefixes" .= toJSON (_ssChanPrefixes ss)
                                   ]

instance FromJSON CommandStateSnapshot where
    parseJSON (Object v) = CSS
      <$> v .:? "defaultPrefix"   .!= _ssDefPrefix    defaultCommandState
      <*> v .:? "channelPrefixes" .!= _ssChanPrefixes defaultCommandState
    parseJSON _ = fail "Bad type"

instance Snapshot CommandState CommandStateSnapshot where
    snapshotSTM state = do
      defPrefix    <- readTVar . _commandPrefix   $ state
      chanPrefixes <- readTVar . _channelPrefixes $ state

      return CSS { _ssDefPrefix    = defPrefix
                 , _ssChanPrefixes = toPrefixTree chanPrefixes
                 }

      where toPrefixTree = fmap M.fromList . M.fromList . collect . map flipTuple

            flipTuple ((host, chan), pref) = (unpack host, (chan, pref))

instance Rollback CommandStateSnapshot CommandState where
    rollbackSTM ss = do
      tvarP  <- newTVar . _ssDefPrefix $ ss
      tvarCP <- newTVar . fromPrefixTree . _ssChanPrefixes $ ss
      tvarL  <- newTVar []

      return CommandState { _commandPrefix   = tvarP
                          , _channelPrefixes = tvarCP
                          , _commandList     = tvarL
                          }
      where fromPrefixTree = concatMap fromNets . M.toList . fmap M.toList

            fromNets (host, chans) = map (fromChans host) chans

            fromChans host (chan, pref) = ((pack host, chan), pref)
