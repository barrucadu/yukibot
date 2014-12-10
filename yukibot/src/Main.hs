{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative    ((<$>))
import Control.Concurrent.STM (atomically, readTVar)
import Control.Monad.Trans.Reader (runReaderT)
import Data.Default.Class     (def)
import Network.IRC.Asakura
import Network.IRC.Asakura.Commands (CommandDef(..), registerCommand)
import Network.IRC.Asakura.State (rollback)
import Network.IRC.Asakura.Types
import Network.IRC.Client
import System.Directory       (doesFileExist)
import System.Environment     (getArgs)
import System.Exit            (exitFailure)
import System.Posix.Signals   (Handler(..), installHandler, sigINT, sigTERM)
import Yukibot.State
import Yukibot.Utils

import qualified Network.IRC.Asakura.Blacklist   as BL
import qualified Network.IRC.Asakura.Commands    as C
import qualified Network.IRC.Asakura.Help        as H
import qualified Network.IRC.Asakura.Permissions as P
import qualified Yukibot.Plugins.Brainfuck       as BF
import qualified Yukibot.Plugins.Cellular        as CA
import qualified Yukibot.Plugins.Channels        as CH
import qualified Yukibot.Plugins.Initialise      as I
import qualified Yukibot.Plugins.LinkInfo        as L
import qualified Yukibot.Plugins.Memory          as M
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
  state <- setKeyStore ys <$> newBotState

  let keyval = _roKeyStore ys

  let ps  = _permissionState ys
  let cs  = _commandState    ys
  let bs  = _blacklistState  ys
  let ls  = _linkinfoState   ys
  let wfs = M.simpleFactStore (defaultMongo' keyval "watching") "watching"

  -- Register signal handlers
  installHandler sigINT  (Catch $ handler state) Nothing
  installHandler sigTERM (Catch $ handler state) Nothing

  -- Register commands
  registerCommand cs $ H.helpCmd cs

  registerCommand cs $ P.wrapsCmd ps (P.Admin 0)   CH.joinCmd
  registerCommand cs $ P.wrapsCmd ps (P.Admin 0)   CH.partCmd
  registerCommand cs $ P.wrapsCmd ps (P.Admin 0) $ CH.setChanPrefix   cs
  registerCommand cs $ P.wrapsCmd ps (P.Admin 0) $ CH.unsetChanPrefix cs
  registerCommand cs $ P.wrapsCmd ps (P.Admin 0) $ BL.blacklistCmd    bs
  registerCommand cs $ P.wrapsCmd ps (P.Admin 0) $ BL.whitelistCmd    bs
  registerCommand cs $ P.wrapsCmd ps (P.Admin 0)   T.addTriggerCmd
  registerCommand cs $ P.wrapsCmd ps (P.Admin 0)   T.rmTriggerCmd
  registerCommand cs   T.listTriggerCmd

  registerCommand cs $ BL.wrapsCmd bs "watching" $ (M.simpleGetCommand wfs) { _verb = ["watching"] }
  registerCommand cs $ BL.wrapsCmd bs "watching" $ (M.simpleSetCommand wfs) { _verb = ["set", "watching"] }
  registerCommand cs $ BL.wrapsCmd bs "seen"        S.command
  registerCommand cs $ BL.wrapsCmd bs "cellular"    CA.command
  registerCommand cs $ BL.wrapsCmd bs "brainfuck"   BF.command

  -- Register event handlers
  addGlobalEventHandler' state $ C.eventRunner cs

  addGlobalEventHandler' state $ BL.wraps bs "seen"       S.eventHandler
  addGlobalEventHandler' state $ BL.wraps bs "linkinfo" $ L.eventHandler ls
  addGlobalEventHandler' state $ BL.wraps bs "triggers"   T.eventHandler

  -- Connect to networks
  let is = _initialState ys
  I.initialiseWithState state is

  -- Block until all networks have been disconnected from
  blockWithState state

  -- Save the state
  save fp ys

-- |Set the key-value store in a botstate from the global
-- configuration.
setKeyStore :: YukibotState -> BotState -> BotState
setKeyStore ys s = s { _keyStore = _roKeyStore ys }

-- |Handle a signal by disconnecting from every IRC network.
handler :: BotState -> IO ()
handler botstate = (atomically . readTVar . _connections $ botstate) >>= mapM_ (runReaderT dc . snd)
    where dc = do
            send . Quit $ Just "Process interrupted."
            disconnect
