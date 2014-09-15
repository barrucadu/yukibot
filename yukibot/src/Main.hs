{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative    ((<$>))
import Control.Concurrent.STM (atomically, readTVar)
import Control.Monad.Trans.Reader (runReaderT)
import Data.Default.Class     (def)
import Network.IRC.Asakura
import Network.IRC.Asakura.State (rollback)
import Network.IRC.Asakura.Types
import Network.IRC.Client
import System.Directory       (doesFileExist)
import System.Environment     (getArgs)
import System.Exit            (exitFailure)
import System.Posix.Signals   (Handler(..), installHandler, sigINT, sigTERM)
import Yukibot.State

import qualified Network.IRC.Asakura.Commands    as C
import qualified Network.IRC.Asakura.Permissions as P
import qualified Yukibot.Plugins.Blacklist       as BL
import qualified Yukibot.Plugins.Cellular        as CA
import qualified Yukibot.Plugins.Channels        as CH
import qualified Yukibot.Plugins.ImgurLinks      as I
import qualified Yukibot.Plugins.Initialise      as I
import qualified Yukibot.Plugins.LinkInfo        as L
import qualified Yukibot.Plugins.LinkInfo.Common as LC
import qualified Yukibot.Plugins.MAL             as M
import qualified Yukibot.Plugins.Memory          as Me
import qualified Yukibot.Plugins.Seen            as S
import qualified Yukibot.Plugins.Trigger         as T

-- |Default configuration file name
defaultConfigFile :: FilePath
defaultConfigFile = "yukibot.json"

-- |Load the configuration file, if it exists, otherwise initialise a
-- new state. Upon successfully constructing a state, run the bot.
main :: IO ()
main = do
  configFile <- do
    args <- getArgs
    return $ case args of
      (cfg:_) -> cfg
      _       -> defaultConfigFile

  confExists <- doesFileExist configFile
  ys <- if confExists
       then stateFromFile configFile
       else Just <$> rollback def

  case ys of
    Just ys' -> runWithState configFile ys'
    Nothing  -> putStrLn "Failed to parse configuration file." >> exitFailure

-- |Run the bot with a given state.
runWithState :: FilePath -> YukibotState -> IO ()
runWithState fp ys = do
  state <- newBotState

  let cs  = _commandState   ys
  let bs  = _blacklistState ys
  let ms  = _memoryState    ys
  let ts  = _triggerState   ys

  let wfs = Me.simpleFactStore ms "watching"
  let lis = LC.addLinkHandler (_linkinfoState ys) I.licPredicate I.licHandler

  -- Register signal handlers
  installHandler sigINT  (Catch $ handler state) Nothing
  installHandler sigTERM (Catch $ handler state) Nothing

  -- Register commands
  C.registerCommand     cs "join"              (Just $ P.Admin 0) CH.joinCmd
  C.registerCommand     cs "part"              (Just $ P.Admin 0) CH.partCmd
  C.registerLongCommand cs ["set",   "prefix"] (Just $ P.Admin 0) $ CH.setChanPrefix   cs
  C.registerLongCommand cs ["unset", "prefix"] (Just $ P.Admin 0) $ CH.unsetChanPrefix cs
  C.registerCommand     cs "blacklist"         (Just $ P.Admin 0) $ BL.blacklistCmd bs
  C.registerCommand     cs "whitelist"         (Just $ P.Admin 0) $ BL.whitelistCmd bs

  C.registerCommand     cs "mal"               Nothing $ BL.wrapsCmd bs "mal"      $ M.malCommand (_malState ys)
  C.registerCommand     cs "watching"          Nothing $ BL.wrapsCmd bs "watching" $ Me.simpleGetCommand wfs
  C.registerLongCommand cs ["set", "watching"] Nothing $ BL.wrapsCmd bs "watching" $ Me.simpleSetCommand wfs
  C.registerCommand     cs "seen"              Nothing $ BL.wrapsCmd bs "seen"     $ S.command ms
  C.registerCommand     cs "rule"              Nothing $ BL.wrapsCmd bs "cellular" CA.command

  -- Register event handlers
  addGlobalEventHandler' state $ C.eventRunner cs

  addGlobalEventHandler' state $ BL.wraps bs "seen"     $ S.eventHandler ms
  addGlobalEventHandler' state $ BL.wraps bs "linkinfo" $ L.eventHandler lis
  addGlobalEventHandler' state $ BL.wraps bs "triggers" $ T.eventHandler ts

  -- Connect to networks
  let is = _initialState ys
  I.initialiseWithState state is

  -- Block until all networks have been disconnected from
  blockWithState state

  -- Save the state
  save fp ys

-- |Handle a signal by disconnecting from every IRC network.
handler :: BotState -> IO ()
handler botstate = (atomically . readTVar . _connections $ botstate) >>= mapM_ (runReaderT dc . snd)
    where dc = do
            send . Quit $ Just "Process interrupted."
            disconnect
