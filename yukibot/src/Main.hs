{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative ((<$>))
import Control.Monad       (void)
import Data.Monoid         ((<>))
import Data.Text           (Text, pack)
import Network             (HostName)
import Network.IRC.Asakura
import Network.IRC.Asakura.Types
import Network.IRC.IDTE

import qualified Network.IRC.Asakura.Commands as C

main :: IO ()
main = do
  cconf <- connect "irc.freenode.net" 6667
  state <- newBotState

  commandState <- C.initialise "#"

  C.registerCommand commandState "echo"  bounceBack
  C.registerCommand commandState "rizon" joinRizon
  C.registerCommand commandState "join"  joinChannel
  C.registerCommand commandState "part"  partChannel

  addGlobalEventHandler' state $ C.eventRunner commandState

  case cconf of
    Right cconf' -> void $ run cconf' (defaultIRCConf "yukibot") state
    Left err     -> putStrLn err

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
