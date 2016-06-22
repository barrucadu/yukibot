module Main where

import Yukibot.Backend
import Yukibot.Backend.IRC

main :: IO ()
main = awaitStop =<< startBackend (\e -> putStrLn $ "GOT EVENT: " ++ show e) ircBackend
