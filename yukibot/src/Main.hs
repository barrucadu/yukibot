{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.HashMap.Strict as H
import Data.Monoid (mconcat)
import Data.Semigroup ((<>))
import Data.Text (Text, unpack)

import Yukibot.Backend
import Yukibot.Backend.IRC
import Yukibot.Configuration

main :: IO ()
main = do
  -- The Core will handle this more gracefully (eventually)
  mcfg <- parseConfigFile "configuration.toml"
  case mcfg of
    Just cfg -> do
      let freenode = do
            (VTable backend)  <- H.lookup "backend" cfg
            (VTable irc)      <- H.lookup "irc" backend
            (VTable freenode) <- H.lookup "irc.freenode.net" irc
            pure freenode
      case ircBackend "irc.freenode.net" <$> freenode of
        Just (Right b) -> do
          h <- startBackend (\e -> putStrLn $ "GOT EVENT: " ++ showTextEvent e) b
          awaitStop h
        Just (Left err) -> putStrLn $ "Error in freenode configuration: " ++ show err
        Nothing -> putStrLn "Freenode configuration not found."
    Nothing -> putStrLn "Parse error in configuration file."

showTextEvent :: Event Text Text -> String
showTextEvent (Event h mc u msg) = unpack . mconcat $
  [ "[@" <> describeBackend h <> "] "
  , maybe "" (\c -> "[in: " <> c <> "] ") mc
  , "[from: " <> u <> "] "
  , msg
  ]
