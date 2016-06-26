{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Exit (die)

import Yukibot.Backend.IRC (ircBackend)
import Yukibot.Core
import Yukibot.Plugin.Hello (helloPlugin)

main :: IO ()
main = case initialState of
  Right st -> defaultMain st "configuration.toml"
  Left err -> die ("Error constructing initial state: " ++ show err)

initialState :: Either CoreError BotState
initialState = addBackend "irc" ircBackend   =<<
               addPlugin "hello" helloPlugin =<<
               pure emptyBotState
