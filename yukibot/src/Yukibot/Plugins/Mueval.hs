{-# LANGUAGE OverloadedStrings #-}

-- |This module emulates Lambdabot's famous evaluation functionality,
-- bringing it to the objectively cuter Yukibot!
module Yukibot.Plugins.Mueval
  ( MuevalCfg
  , command
  , eventHandler
  ) where

import Control.Applicative          ((<$>), (<*>))
import Control.Monad.IO.Class       (MonadIO, liftIO)
import Data.Aeson                   (FromJSON(..), ToJSON(..), Value(..), (.=), (.:?), (.!=), object)
import Data.Char                    (isSpace)
import Data.Monoid                  ((<>))
import Data.Text                    (isPrefixOf, unpack, pack)
import Network.IRC.Asakura.Commands (CommandDef(..))
import Network.IRC.Asakura.Events   (runAlways, runEverywhere)
import Network.IRC.Asakura.Types    (AsakuraEventHandler(..))
import Network.IRC.Client           (reply)
import Network.IRC.Client.Types     (Event(..), EventType(EPrivmsg), Message(Privmsg), UnicodeEvent, IRC)
import System.Process               (readProcessWithExitCode)
import System.Random                (randomIO)
import Yukibot.Utils

import qualified Data.Text as T

-- *Configuration

data MuevalCfg = MC
    { _mueval :: String
    -- ^Path to the mueval binary
    , _loadfile :: String
    -- ^Path to the load file
    }

instance ToJSON MuevalCfg where
  toJSON mc = object [ "mueval"   .= _mueval   mc
                     , "loadfile" .= _loadfile mc
                     ]

instance FromJSON MuevalCfg where
  parseJSON (Object v) = MC <$> v .:? "mueval"   .!= _mueval   defaultMuevalCfg
                            <*> v .:? "loadfile" .!= _loadfile defaultMuevalCfg
  parseJSON _ = fail "Expected object"

defaultMuevalCfg :: MuevalCfg
defaultMuevalCfg = MC "mueval" "L.hs"

-- *External usage

-- |Evaluate expressions as a command
command :: CommandDef
command = CommandDef
             { _verb = ["eval"]
             , _help = "<expr> - Evaluate the given expression"
             , _action = go
             }
  where
    go args _ ev = do
      mc <- cfgGet' defaultMuevalCfg "mueval"
      res <- mueval mc . unpack $ T.intercalate " " args
      return $ replyOrPaste ev res

-- |Evaluate expressions in PRIVMSGs starting with a '>'
eventHandler :: AsakuraEventHandler
eventHandler = AsakuraEventHandler
                  { _description = pack "A sandboxed evaluator for Haskell expressions."
                  , _matchType   = EPrivmsg
                  , _eventFunc   = go
                  , _appliesTo   = runEverywhere
                  , _appliesDef  = runAlways
                  }
  where
    go _ ev =
      case _message ev of
        Privmsg _ (Right msg) | "> " `isPrefixOf` msg -> do
          let expr = T.strip . T.drop 1 $ msg
          mc <- cfgGet' defaultMuevalCfg "mueval"
          res <- mueval mc $ unpack expr
          return $ replyOrPaste ev res
        _ -> return $ return ()

-- *Evaluation

-- |Evaluate an expression and return the result.
mueval :: MonadIO m => MuevalCfg -> String -> m String
mueval mc expr = do
  -- Generate a random seed
  seed <- liftIO (randomIO :: IO Int)
  let expr' = "let seed = " ++ show seed ++ " in " ++ expr

  let loadfile = _loadfile mc
  let binary   = _mueval mc
  let opts     = muopts loadfile expr'

  (_, out, err) <- liftIO $ readProcessWithExitCode binary opts ""
  return . strip $
    case (out, err) of
      ([], []) -> "Terminated"

      _ -> do
        let o = strip out
        let e = strip out
        case () of
          _ | null o && null e -> "Terminated"
            | null o          -> e
            | otherwise       -> o

-- |Default options for mueval
muopts :: String -> String -> [String]
muopts loadfile expr =
  [ "--no-imports"
  , "-l", loadfile
  , "--expression=" ++ expr
  , "+RTS", "-N", "-RTS"
  ]

-- *Misc

-- |Reply (if the output is single line), or upload to sprunge (if
-- multi-line) and reply with link.
replyOrPaste :: UnicodeEvent -> String -> IRC ()
replyOrPaste ev txt | '\n' `elem` txt = paste txt >>= \p -> reply ev $ "Multi-line result: " <> p
                    | otherwise       = reply ev . pack $ shrink txt

-- |Shrink a long output and add an ellipsis
shrink :: String -> String
shrink str | length str > 80 = take 79 str ++ "…"
shrink str = str

-- |Strip leading and trailing whitespace from a string.
strip :: String -> String
strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace
