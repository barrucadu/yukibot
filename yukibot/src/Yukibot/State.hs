{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Yukibot.State where

import Control.Applicative      ((<$>), (<*>))
import Control.Monad            (join)
import Control.Monad.IO.Class   (MonadIO, liftIO)
import Data.Aeson               (FromJSON(..), ToJSON(..), Value(..), (.=), (.:?), (.!=), object, decode')
import Data.Aeson.Encode.Pretty (Config(..), encodePretty')
import Data.ByteString.Lazy     (ByteString)
import Data.Default.Class       (Default(..))
import Data.Text                ()
import Network.IRC.Asakura.State
import System.Directory         (doesFileExist)

import qualified Data.ByteString.Lazy            as B
import qualified Network.IRC.Asakura.Commands    as C
import qualified Network.IRC.Asakura.Permissions as P
import qualified Yukibot.Plugins.Blacklist       as BL
import qualified Yukibot.Plugins.Initialise      as I
import qualified Yukibot.Plugins.LinkInfo        as L
import qualified Yukibot.Plugins.MAL             as M
import qualified Yukibot.Plugins.Memory          as Me
import qualified Yukibot.Plugins.Trigger         as T

-- *Live State

-- |The live state of the bot.
data YukibotState = YS
    { _commandState    :: C.CommandState
    , _permissionState :: P.PermissionState
    , _linkinfoState   :: L.LinkInfoCfg
    , _malState        :: M.MALCfg
    , _memoryState     :: Me.MemoryState
    , _triggerState    :: T.TriggerState
    , _blacklistState  :: BL.BlacklistState
    , _initialState    :: I.InitialCfg
    }

-- *Snapshotting

-- |A snapshot of the bot.
data YukibotStateSnapshot = YSS
    { _commandSnapshot    :: C.CommandStateSnapshot
    , _permissionSnapshot :: P.PermissionStateSnapshot
    , _linkinfoSnapshot   :: L.LinkInfoCfg
    , _malSnapshot        :: M.MALCfg
    , _memorySnapshot     :: Me.MemoryStateSnapshot
    , _triggerSnapshot    :: T.TriggerStateSnapshot
    , _blacklistSnapshot  :: BL.BlacklistStateSnapshot
    , _initialSnapshot    :: I.InitialCfg
    }

instance Default YukibotStateSnapshot where
    def = YSS def def def def def def def def

instance Snapshot YukibotState YukibotStateSnapshot where
    snapshotSTM ys = do
      css <- snapshotSTM . _commandState    $ ys
      pss <- snapshotSTM . _permissionState $ ys
      mss <- snapshotSTM . _memoryState     $ ys
      tss <- snapshotSTM . _triggerState    $ ys
      bss <- snapshotSTM . _blacklistState  $ ys

      return YSS { _commandSnapshot    = css
                 , _permissionSnapshot = pss
                 , _linkinfoSnapshot   = _linkinfoState ys
                 , _malSnapshot        = _malState ys
                 , _memorySnapshot     = mss
                 , _triggerSnapshot    = tss
                 , _blacklistSnapshot  = bss
                 , _initialSnapshot    = _initialState ys
                 }

instance Rollback YukibotStateSnapshot YukibotState where
    rollbackSTM yss = do
      cs <- rollbackSTM . _commandSnapshot    $ yss
      ps <- rollbackSTM . _permissionSnapshot $ yss
      ms <- rollbackSTM . _memorySnapshot     $ yss
      ts <- rollbackSTM . _triggerSnapshot    $ yss
      bs <- rollbackSTM . _blacklistSnapshot  $ yss

      return YS { _commandState    = cs
                , _permissionState = ps
                , _linkinfoState   = _linkinfoSnapshot yss
                , _malState        = _malSnapshot yss
                , _memoryState     = ms
                , _triggerState    = ts
                , _blacklistState  = bs
                , _initialState    = _initialSnapshot yss
                }

instance ToJSON YukibotStateSnapshot where
    toJSON yss = object [ "prefixes"    .= toJSON (_commandSnapshot    yss)
                        , "permissions" .= toJSON (_permissionSnapshot yss)
                        , "linkinfo"    .= toJSON (_linkinfoSnapshot   yss)
                        , "myanimelist" .= toJSON (_malSnapshot        yss)
                        , "memory"      .= toJSON (_memorySnapshot     yss)
                        , "triggers"    .= toJSON (_triggerSnapshot    yss)
                        , "blacklist"   .= toJSON (_blacklistSnapshot  yss)
                        , "initial"     .= toJSON (_initialSnapshot    yss)
                        ]

instance FromJSON YukibotStateSnapshot where
    parseJSON (Object v) = YSS <$> v .:? "prefixes"    .!= def
                               <*> v .:? "permissions" .!= def
                               <*> v .:? "linkinfo"    .!= def
                               <*> v .:? "myanimelist" .!= def
                               <*> v .:? "memory"      .!= def
                               <*> v .:? "triggers"    .!= def
                               <*> v .:? "blacklist"   .!= def
                               <*> v .:? "initial"     .!= def
    parseJSON _ = fail "Expected object"

-- *Initialisation

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
