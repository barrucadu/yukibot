{-# LANGUAGE OverloadedStrings #-}

-- |This module emulates Lambdabot's famous evaluation functionality,
-- bringing it to the objectively cuter Yukibot!
module Yukibot.Plugins.Mueval
  ( MuevalCfg
  -- * Evaluation
  , evalCommand
  , evalEvent
  -- * Type lookup
  , typeCommand
  , kindCommand
  , typeEvent
  , kindEvent
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, takeMVar, putMVar)
import Control.Exception (evaluate)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (FromJSON(..), ToJSON(..), Value(..), (.=), (.:?), (.!=), object)
import Data.Char (isSpace)
import Data.Monoid ((<>))
import Data.Text (isPrefixOf, unpack, pack)
import Network.IRC.Bot.Commands (CommandDef(..))
import Network.IRC.Bot.Events (runAlways, runEverywhere)
import Network.IRC.Bot.Types (EventHandler(..))
import Network.IRC.Client (reply)
import Network.IRC.Client.Types (Event(..), EventType(EPrivmsg), Message(Privmsg), UnicodeEvent, IRC)
import System.Environment (getEnvironment)
import System.Exit (ExitCode)
import System.IO (hGetContents, hPutStr, hClose)
import System.Process (CreateProcess(..), StdStream(CreatePipe), createProcess, proc, waitForProcess)
import System.Random (randomIO)

import Yukibot.Utils

import qualified Data.List as L
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
  parseJSON (Object v) = MC
    <$> v .:? "mueval"   .!= _mueval   defaultMuevalCfg
    <*> v .:? "loadfile" .!= _loadfile defaultMuevalCfg
  parseJSON _ = fail "Expected object"

defaultMuevalCfg :: MuevalCfg
defaultMuevalCfg = MC "mueval" "L.hs"

-- *External usage

-- |Evaluate expressions as a command
evalCommand :: CommandDef
evalCommand = CommandDef
  { _verb = ["eval"]
  , _help = "<expr> - Evaluate the given expression"
  , _action = go
  }

  where
    go args _ ev = do
      mc <- cfgGet' defaultMuevalCfg "mueval"
      res <- mueval mc . unpack $ T.intercalate " " args
      return $ replyOrPaste ev res

-- |Get the type of an expression as a command
typeCommand :: CommandDef
typeCommand = CommandDef
  { _verb = ["type"]
  , _help = "<expr> - Get the type of the given expression"
  , _action = go
  }

  where
    go args _ ev = do
      mc <- cfgGet' defaultMuevalCfg "mueval"
      res <- typeOf mc . unpack $ T.intercalate " " args
      return $ replyOrPaste ev res

-- |Get the kind of an expression as a command
kindCommand :: CommandDef
kindCommand = CommandDef
  { _verb = ["kind"]
  , _help = "<expr> - Get the kind of the given expression"
  , _action = go
  }

  where
    go args _ ev = do
      mc <- cfgGet' defaultMuevalCfg "mueval"
      res <- kindOf mc . unpack $ T.intercalate " " args
      return $ replyOrPaste ev res

-- |Evaluate expressions in PRIVMSGs starting with a '>'
evalEvent :: EventHandler
evalEvent = EventHandler
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

-- |Get the type of expressions in PRIVMSGs starting with a ':t'
typeEvent :: EventHandler
typeEvent = EventHandler
  { _description = pack "Get the type of a Haskell expression."
  , _matchType = EPrivmsg
  , _eventFunc = go
  , _appliesTo = runEverywhere
  , _appliesDef = runAlways
  }

  where
    go _ ev = case _message ev of
      Privmsg _ (Right msg) | ":t " `isPrefixOf` msg -> do
        let expr = unpack . T.strip . T.drop 2 $ msg
        mc <- cfgGet' defaultMuevalCfg "mueval"
        res <- typeOf mc expr
        return $ replyOrPaste ev res
      _ -> return $ return ()

-- |Get the kind of expressions in PRIVMSGs starting with a ':k'
kindEvent :: EventHandler
kindEvent = EventHandler
  { _description = pack "Get the kind of a Haskell expression."
  , _matchType = EPrivmsg
  , _eventFunc = go
  , _appliesTo = runEverywhere
  , _appliesDef = runAlways
  }

  where
    go _ ev = case _message ev of
      Privmsg _ (Right msg) | ":k " `isPrefixOf` msg -> do
        let expr = unpack . T.strip . T.drop 2 $ msg
        mc <- cfgGet' defaultMuevalCfg "mueval"
        res <- kindOf mc expr
        return $ replyOrPaste ev res
      _ -> return $ return ()

-- *Evaluation

-- |Evaluate an expression and return the result.
mueval :: MonadIO m => MuevalCfg -> String -> m String
mueval mc expr = liftIO $ do
  -- Generate a random seed
  seed <- randomIO :: IO Int
  let expr' = "let seed = " ++ show seed ++ " in " ++ expr

  let loadfile = _loadfile mc
  let binary   = _mueval mc
  let opts     = muopts loadfile expr'
  let input    = ""

  env  <- (("LC_ALL", "C"):) <$> getEnvironment
  (_, out, err) <- runProcess binary opts (Just env) input
  return . strip $
    case (out, err) of
      ([], []) -> "Terminated"

      _ -> do
        let o = strip out
        let e = strip err
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
  , "+RTS", "-N2", "-RTS"
  ]

-- *Types and Kinds

-- |Get the type of an expression
typeOf :: MonadIO m => MuevalCfg -> String -> m String
typeOf = queryGHCi ":t"

-- |Get the kind of an expression
kindOf :: MonadIO m => MuevalCfg -> String -> m String
kindOf = queryGHCi ":k"

queryGHCi :: MonadIO m => String -> MuevalCfg -> String -> m String
queryGHCi cmd mc expr = liftIO $ do
  let loadfile = _loadfile mc
  let binary   = "ghci"
  let opts     = ["-fforce-recomp", "-ignore-dot-ghci"]
  let input    = ":load " ++ loadfile ++ "\n:m *L\n" ++ cmd ++ " " ++ expr

  env <- (("LC_ALL", "C"):) <$> getEnvironment
  (_, out, err) <- runProcess binary opts (Just env) input
  return . strip $
    case (out, err) of
      ([], []) -> "Terminated"

      _ -> do
        let o = strip out
        let e = strip err
        case () of
          _ | null o && null e    -> "Terminated"
            | L.isInfixOf "::" o -> drop (length ("*L> *L>" :: String)) . unlines . filter (L.isInfixOf "::") $ lines o
            | otherwise          -> e

-- *Misc

-- |Run a process, returning the exit code, stdout, and stderr.
--
-- Copied from 'readProcessWithExitCode', but with environment
-- setting.
runProcess :: FilePath -> [String] -> Maybe [(String, String)] -> String -> IO (ExitCode, String, String)
runProcess cmd args env input = do
  -- Create the process
  (Just inh, Just outh, Just errh, pid) <-
    createProcess (proc cmd args)
    { std_in  = CreatePipe
    , std_out = CreatePipe
    , std_err = CreatePipe
    , env     = env
    }

  outMVar <- newEmptyMVar

  -- New thread to read stdout
  out  <- hGetContents outh
  forkIO $ evaluate (length out) >> putMVar outMVar ()

  -- New thread to read stderr
  err  <- hGetContents errh
  forkIO $ evaluate (length err) >> putMVar outMVar ()

  -- Write stdin
  unless (null input) $ hPutStr inh input
  hClose inh

  -- Wait on the output
  takeMVar outMVar
  takeMVar outMVar
  hClose outh
  hClose errh

  -- Wait for termination
  ex <- waitForProcess pid

  return (ex, out, err)

-- |Reply (if the output is single line), or upload to sprunge (if
-- multi-line) and reply with link.
replyOrPaste :: UnicodeEvent -> String -> IRC ()
replyOrPaste ev txt
  | length (lines txt) == 1 = reply ev . pack $ shrink txt
  | length (lines txt) == 2 = reply ev $ pack txt
  | otherwise = paste txt >>= \p -> reply ev $ "Multi-line result: " <> p

-- |Shrink a long output and add an ellipsis
shrink :: String -> String
shrink str | length str > 80 = take 79 str ++ "…"
shrink str = str

-- |De-indent a multi-line string, as well as dropping a first line
-- starting with \"<hint>\" or \"<interactive>\".
strip :: String -> String
strip = dropSpaces . deIndent . dropComplaint where
  dropComplaint = unlines . filter (\l -> not $ L.isPrefixOf "<interactive>" l || L.isPrefixOf "<hint>" l) . lines
  deIndent s = let ls = lines s in unlines $ map (drop $ getMinSpaces ls) ls where
    getMinSpaces = go Nothing where
      go s ("":xs) = go s xs
      go Nothing [] = 0
      go (Just s) [] = s
      go Nothing (x:xs) = go (Just $ spaces x) xs
      go (Just s) (x:xs) = let s' = spaces x in if s < s' then go (Just s) xs else go (Just s') xs

      spaces = length . takeWhile (==' ')
  dropSpaces = reverse . dropWhile isSpace . reverse . dropWhile isSpace
