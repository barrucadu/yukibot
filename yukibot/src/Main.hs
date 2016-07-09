{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Exit (die)

import Yukibot.Backend.IRC (ircBackend)

import Yukibot.Core

import Yukibot.Plugin.Channel  (channelPlugin)
import Yukibot.Plugin.Hello    (helloPlugin)
import Yukibot.Plugin.LinkInfo (linkInfoPlugin)
import Yukibot.Plugin.Mueval   (muevalPlugin)
import Yukibot.Plugin.Seen     (seenPlugin)
import Yukibot.Plugin.Trigger  (triggerPlugin)

main :: IO ()
main = case initialState of
  Right st -> defaultMain st "configuration.toml"
  Left err -> die ("Error constructing initial state: " ++ show err)

initialState :: Either CoreError BotState
initialState = addBackend "irc"     ircBackend     =<<
               addPlugin "channel"  channelPlugin  =<<
               addPlugin "hello"    helloPlugin    =<<
               addPlugin "linkinfo" linkInfoPlugin =<<
               addPlugin "mueval"   muevalPlugin   =<<
               addPlugin "seen"     seenPlugin     =<<
               addPlugin "trigger"  triggerPlugin  =<<
               pure emptyBotState
