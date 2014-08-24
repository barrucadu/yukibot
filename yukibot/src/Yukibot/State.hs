{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Yukibot.State where

import Control.Applicative      ((<$>), (<*>))
import Control.Monad            (join)
import Control.Monad.IO.Class   (MonadIO, liftIO)
import Data.Aeson               (FromJSON(..), ToJSON(..), Value(..), (.=), (.:), (.:?), (.!=), object, decode')
import Data.Aeson.Encode.Pretty (Config(..), encodePretty')
import Data.ByteString.Lazy     (ByteString)
import Data.Default.Class       (def)
import Data.Text                ()
import Network.IRC.Asakura.State
import System.Directory         (doesFileExist)

import qualified Data.ByteString.Lazy            as B
import qualified Network.IRC.Asakura.Commands    as C
import qualified Network.IRC.Asakura.Permissions as P
import qualified Yukibot.Plugins.LinkInfo        as L

-- *Live State

-- |The live state of the bot.
data YukibotState = YS
    { _commandState    :: C.CommandState
    , _permissionState :: P.PermissionState
    , _linkinfoState   :: L.LinkInfoCfg
    }

-- *Snapshotting

-- |A snapshot of the bot.
data YukibotStateSnapshot = YSS
    { _commandSnapshot    :: C.CommandStateSnapshot
    , _permissionSnapshot :: P.PermissionStateSnapshot
    , _linkinfoSnapshot   :: L.LinkInfoCfg
    }

instance Snapshot YukibotState YukibotStateSnapshot where
    snapshotSTM ys = do
      css <- snapshotSTM . _commandState    $ ys
      pss <- snapshotSTM . _permissionState $ ys

      return YSS { _commandSnapshot    = css
                 , _permissionSnapshot = pss
                 , _linkinfoSnapshot   = _linkinfoState ys
                 }

instance Rollback YukibotStateSnapshot YukibotState where
    rollbackSTM yss = do
      cs <- rollbackSTM . _commandSnapshot    $ yss
      ps <- rollbackSTM . _permissionSnapshot $ yss

      return YS { _commandState    = cs ps
                , _permissionState = ps
                , _linkinfoState   = _linkinfoSnapshot yss
                }

instance ToJSON YukibotStateSnapshot where
    toJSON yss = object [ "prefixes"    .= toJSON (_commandSnapshot yss)
                        , "permissions" .= toJSON (_permissionSnapshot yss)
                        , "linkinfo"    .= toJSON (_linkinfoSnapshot yss)
                        ]

instance FromJSON YukibotStateSnapshot where
    parseJSON (Object v) = YSS <$> v .:  "prefixes"
                               <*> v .:  "permissions"
                               <*> v .:? "linkinfo" .!= def
    parseJSON _ = fail "Expected object"

-- *Initialisation

-- |Construct a new state, with a default command prefix of "!".
initialise :: MonadIO m => m YukibotState
initialise = do
  ps <- P.initialise
  cs <- C.initialise "!" ps

  return YS { _commandState    = cs
            , _permissionState = ps
            , _linkinfoState   = def
            }

-- |Attempt to load a state from a lazy ByteString.
stateFromByteString :: (Functor m, MonadIO m) => ByteString -> m (Maybe YukibotState)
stateFromByteString bs = case (decode' bs :: Maybe YukibotStateSnapshot) of
                           Just yss -> Just <$> rollback yss
                           Nothing  -> return Nothing

-- |Attempt to load a state from a file.
stateFromFile :: MonadIO m => FilePath -> m (Maybe YukibotState)
stateFromFile fp = liftIO $ do
  exists <- doesFileExist fp
  if exists
  then join $ stateFromByteString <$> B.readFile fp
  else return Nothing

-- *Saving

-- |Snapshot the state and save it to disk.
save :: (Functor m, MonadIO m) => FilePath -> YukibotState -> m ()
save fp ys = join $ saveSnapshot fp <$> snapshot ys

-- |Save a snapshot of the state to disk.
saveSnapshot :: MonadIO m => FilePath -> YukibotStateSnapshot -> m ()
saveSnapshot fp yss = liftIO $ B.writeFile fp bs
    where bs = encodePretty' (Config 4 compare) yss
