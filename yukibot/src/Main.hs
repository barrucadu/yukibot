{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative ((<$>))
import Control.Monad       (void)
import Data.Monoid         ((<>))
import Data.Text           (pack)
import Network             (HostName)
import Network.IRC.Asakura
import Network.IRC.Asakura.Types
import Network.IRC.IDTE

main :: IO ()
main = do
  cconf <- connect "irc.freenode.net" 6667
  state <- newBotState

  addGlobalEventHandler' state $ AsakuraEventHandler "Respond to everything" EEverything bounceBack runEverywhere runAlways

  case cconf of
    Right cconf' -> void $ run cconf' (defaultIRCConf "yukibot") state
    Left err     -> putStrLn err

bounceBack :: IRCState -> Event -> Bot (IRC ())
bounceBack ircstate ev = do
  nck <- _nick <$> getInstanceConfig' ircstate
  let net = _server $ getConnectionConfig ircstate

  case _eventType ev of
    ENotice -> return $ return () -- Notices don't produce automatic responses.
    _       -> case _source ev of
                Channel n c | n /= nck   -> return . send $ privmsg c $ n <> ": " <> msg
                            | otherwise -> return $ return () -- Don't reply to messages from self.
                User    n   | n == "barrucadu" && net /= "irc.rizon.net" -> joinRizon
                            | n /= nck   -> return . send $ query n msg
                            | otherwise -> return $ return ()
                _                       -> return $ return ()

    where msg = pack . show . _message $ ev

joinRizon :: Bot (IRC ())
joinRizon = do
  cconf <- connect "irc.rizon.net" 6667
  case cconf of
    Right cconf' -> addNetwork cconf' $ defaultIRCConf "yukibot"
    Left err -> error err
  return $ return ()
