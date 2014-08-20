{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative ((<$>))
import Data.Monoid         ((<>))
import Data.Text           (pack)
import Network.IRC.IDTE

main :: IO ()
main = do
  cconf <- connect "irc.freenode.net" 6667
  case cconf of
    Right cconf' -> do
      let iconf  = defaultIRCConf "yukibot"
      let iconf' = iconf { _eventHandlers = EventHandler "Reply to everything" EEverything bounceBack : _eventHandlers iconf }
      run cconf' iconf'
    Left err -> error err

bounceBack :: Event -> IRC ()
bounceBack ev = do
  nck <- _nick <$> instanceConfig
  case _eventType ev of
    ENotice -> return () -- Notices don't produce automatic responses.
    _       -> case _source ev of
                Channel n c | n /= nck   -> send $ privmsg c $ n <> ": " <> msg
                            | otherwise -> return () -- Don't reply to messages from self.
                User    n   | n /= nck   -> send $ query n msg
                            | otherwise -> return ()
                _                       -> return ()

    where msg = pack . show . _message $ ev

