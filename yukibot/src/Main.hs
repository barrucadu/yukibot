{-# LANGUAGE OverloadedStrings #-}

module Main where

import Yukibot.Backend.IRC (ircBackend)
import Yukibot.Configuration (parseConfigFile)
import Yukibot.Core

main :: IO ()
main = do
  mcfg <- parseConfigFile "configuration.toml"
  case (initialState, mcfg) of
    (Right st, Just cfg) -> case makeBot st cfg of
      Right run   -> run
      Left  errs  -> putStrLn $ "Error creating bot: " ++ show errs
    (Left err, _) -> putStrLn $ "Error constructing initial state: " ++ show err
    (_, Nothing)  -> putStrLn   "Error parsing configuration file."

-- TODO: Plugins
initialState :: Either CoreError BotState
initialState = addBackend "irc" ircBackend initialBotState
