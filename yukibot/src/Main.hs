{-# LANGUAGE OverloadedStrings #-}

-- TODO: Catch SIGTERM and disconnect from all networks.
module Main where

import Control.Applicative ((<$>))
import Control.Monad       (void)
import Data.Monoid         ((<>))
import Data.Text           (Text, pack)
import Network             (HostName)
import Network.IRC.Asakura
import Network.IRC.Asakura.State (snapshot, rollback)
import Network.IRC.Asakura.Types
import Network.IRC.IDTE
import System.Directory    (doesFileExist)
import System.Exit         (exitFailure)
import Yukibot.State

import qualified Network.IRC.Asakura.Commands    as C
import qualified Network.IRC.Asakura.Permissions as P

-- |Load the configuration file, if it exists, otherwise initialise a
-- new state. Upon successfully constructing a state, run the bot.
main :: IO ()
main = do
  confExists <- doesFileExist "yukibot.json"
  ys <- if confExists
       then stateFromFile "yukibot.json"
       else Just <$> initialise

  case ys of
    Just ys' -> runWithState ys'
    Nothing  -> putStrLn "Failed to parse configuration file." >> exitFailure

-- |Run the bot with a given state.
runWithState :: YukibotState -> IO ()
runWithState ys = do
  cconf <- connect "irc.freenode.net" 6667
  state <- newBotState

  let commandState    = _commandState ys
  let permissionState = _permissionState ys

  C.registerCommand commandState "echo"  Nothing            bounceBack
  C.registerCommand commandState "rizon" (Just P.God)       joinRizon
  C.registerCommand commandState "join"  (Just $ P.Admin 0) joinChannel
  C.registerCommand commandState "part"  (Just $ P.Admin 0) partChannel
  C.registerCommand commandState "quit"  (Just P.God)       quitNetwork

  addGlobalEventHandler' state $ C.eventRunner commandState

  case cconf of
    Right cconf' -> do
      void $ run cconf' (defaultIRCConf "yukibot") state
      save "yukibot.json" ys

    Left err -> putStrLn err >> exitFailure

bounceBack :: [Text] -> IRCState -> Event -> Bot (IRC ())
bounceBack _ _ ev = case _source ev of
                      Channel n c -> return . send $ privmsg c $ n <> ": " <> msg
                      User    n   -> return . send $ query n msg
                      _           -> return $ return ()
    where msg = pack . show . _message $ ev

joinRizon :: [Text] -> IRCState -> Event -> Bot (IRC ())
joinRizon _ _ _ = do
  cconf <- connect "irc.rizon.net" 6667
  case cconf of
    Right cconf' -> addNetwork cconf' $ defaultIRCConf "yukibot"
    Left err -> error err
  return $ return ()

joinChannel :: [Text] -> IRCState -> Event -> Bot (IRC ())
joinChannel (chan:_) _ _ = return . send $ join chan

partChannel :: [Text] -> IRCState -> Event -> Bot (IRC ())
partChannel _ _ ev = case _source ev of
                       Channel _ c -> return $ leaveChannel c $ Just "Banished by magic"
                       _ -> return $ return ()

quitNetwork :: [Text] -> IRCState -> Event -> Bot (IRC ())
quitNetwork _ _ _ = return $ do
  send $ quit (Just "I must now die, in order that the JSON be produced. Farewell.")
  disconnect
