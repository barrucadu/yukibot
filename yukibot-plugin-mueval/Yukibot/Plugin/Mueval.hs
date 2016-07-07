{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Yukibot.Plugin.Mueval
-- Copyright   : (c) 2016 Michael Walker
-- License     : MIT
-- Stability   : experimental
-- Portability : OverloadedStrings
--
-- Bringing lambdabot's famous evaluation functionality to
-- yukibot!
--
-- There is one monitor:
--
--     * "inline", automatically interpret lines starting with "> ",
--       ":t ", and ":k ".
--
-- Three commands:
--
--     * "eval", evaluate an expression.
--
--     * "type", get the type of an expression.
--
--     * "kind", get the kind of a type.
--
-- And some configuration:
--
--     * "use-stack" (bool), use "stack exec ghci/mueval" rather than
--       GHCi and mueval directly.
--
--     * "ghci-path" (string), path to GHCi binary.
--
--     * "mueval-path" (string), path to mueval binary.
--
--     * "stack-path" (string), path to stack binary.
--
--     * "load-file" (string), path to load file.
module Yukibot.Plugin.Mueval where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, takeMVar, putMVar)
import Control.Exception (SomeException, catch, evaluate)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Lazy (toStrict)
import qualified Data.HashMap.Strict as H
import Data.List (isInfixOf, isPrefixOf)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Network.HTTP.Client.MultipartFormData as WM
import qualified Network.HTTP.Simple as W
import System.Environment (getEnvironment)
import System.Exit (ExitCode)
import System.IO (hGetContents, hPutStr, hClose)
import System.Process (CreateProcess(..), StdStream(CreatePipe), createProcess, proc, waitForProcess)
import System.Random (randomIO)

import Yukibot.Core

muevalPlugin :: Table -> Either error Plugin
muevalPlugin cfg = Right Plugin
  { pluginHelp = "bringing Haskell evaluation to a channel near you"
  , pluginMonitors = H.fromList [("inline", inlineMonitor cfg)]
  , pluginCommands = H.fromList
    [ ("eval", evalCommand cfg)
    , ("type", typeCommand cfg)
    , ("kind", kindCommand cfg)
    ]
  }

-- | Automatically interpret lines starting with '> ', ':t ', and ':k '.
inlineMonitor :: Table -> Monitor
inlineMonitor cfg = Monitor
  { monitorHelp = "automatically interpret lines starting with '> ', ':t ', and ':k '"
  , monitorAction = \ev -> case eventMessage ev of
      msg | "> "  `T.isPrefixOf` msg -> mueval cfg (T.strip $ T.drop 1 msg)
          | ":t " `T.isPrefixOf` msg -> queryGHCi cfg ":t" (T.strip $ T.drop 2 msg)
          | ":k " `T.isPrefixOf` msg -> queryGHCi cfg ":k" (T.strip $ T.drop 2 msg)
      _ -> pure ()
  }

-- | Evaluate Haskell expressions.
evalCommand :: Table -> Command
evalCommand cfg = Command
  { commandHelp = "evaluate Haskell expressions"
  , commandAction = const (mueval cfg . T.unwords)
  }

-- | Get the type of a Haskell expression.
typeCommand :: Table -> Command
typeCommand cfg = Command
  { commandHelp = "get the type of a Haskell expression"
  , commandAction = const (queryGHCi cfg ":t" . T.unwords)
  }

-- | Get the kind of a Haskell type.
kindCommand :: Table -> Command
kindCommand cfg = Command
  { commandHelp = "get the kind of a Haskell type"
  , commandAction = const (queryGHCi cfg ":k" . T.unwords)
  }

-------------------------------------------------------------------------------
-- Evaluation

-- | Evaluate an expression.
mueval :: Table -> Text -> BackendM ()
mueval cfg expr = do
  (_, out, _) <- liftIO $ do
    seed <- randomIO :: IO Int
    let expr' = "let seed = " ++ show seed ++ " in " ++ T.unpack expr

    env <- (("LC_ALL", "C"):) <$> getEnvironment
    runProcess binary (opts ++ baseOpts expr') (Just env) ""

  -- mueval doesn't use stderr, grr
  niceErr <- liftIO (formatErrors out)
  quickReply . T.pack $
    if "error:" `isPrefixOf` out then niceErr else formatExpr out

  where
    binary     = T.unpack (if useStack then stackPath else muevalPath)
    opts       = if useStack then ["exec", T.unpack muevalPath, "--"] else []
    useStack   = getBool "use-stack" cfg == Just True
    muevalPath = fromMaybe "mueval" (getString "mueval-path" cfg)
    stackPath  = fromMaybe "stack"  (getString "stack-path"  cfg)
    baseOpts e = catMaybes
      [ Just "--no-imports"
      , (("-l"++) . T.unpack) <$> getString "load-file" cfg
      , Just ("--expression=" ++ e)
      , Just "+RTS", Just "-N2", Just "-RTS"
      ]

-- | Query GHCi.
queryGHCi :: Table -> String -> Text -> BackendM ()
queryGHCi cfg cmd expr = do
  (_, out, err) <- liftIO $ do
    env <- (("LC_ALL", "C"):) <$> getEnvironment
    runProcess binary (opts ++ baseOpts) (Just env) input

  niceErr <- liftIO (formatErrors err)

  quickReply . T.pack $
    if null niceErr then formatTypeOrKind out else niceErr

  where
    input = T.unpack $ case getString "load-file" cfg of
      Just loadFile | not (T.null loadFile) ->
        ":load " <> loadFile <> "\n:m *L\n" <> T.pack cmd <> " " <> expr
      _ -> T.pack cmd <> " " <> expr

    binary    = T.unpack (if useStack then stackPath else ghciPath)
    opts      = if useStack then ["exec", T.unpack ghciPath, "--"] else []
    useStack  = getBool "use-stack" cfg == Just True
    ghciPath  = fromMaybe "ghci"  (getString "ghci-path"  cfg)
    stackPath = fromMaybe "stack" (getString "stack-path" cfg)
    baseOpts  = ["-fforce-recomp", "-ignore-dot-ghci"]

-------------------------------------------------------------------------------
-- Processes

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
  _    <- forkIO $ evaluate (length out) >> putMVar outMVar ()

  -- New thread to read stderr
  err  <- hGetContents errh
  _    <- forkIO $ evaluate (length err) >> putMVar outMVar ()

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

  pure (ex, out, err)

-------------------------------------------------------------------------------
-- Output

-- | Format an expression: truncate to 80 characters.
formatExpr :: String -> String
formatExpr str =
  let str' = filter (/='\n') str
  in if length str' > 80
     then take 79 str' ++ "…"
     else str'

-- | Format a type or kind: drop all lines other than the first
-- containing " :: " and strip the module prefix.
formatTypeOrKind :: String -> String
formatTypeOrKind = go . filter (" :: " `isInfixOf`) . lines where
  go [line] | l `isPrefixOf` line = go [drop (length l) line]
            | p `isPrefixOf` line = go [drop (length p) line]
            | otherwise = line
  go _ = ""

  l, p :: String
  l = "*L> "
  p = "Prelude> "

-- | Format some errors: return the first sentence of the first error
-- message, and upload the full output if there is more than one.
formatErrors :: String -> IO String
formatErrors errStr
  | null errors = pure ""
  | numErrs <= 1 = pure firstSentence
  | otherwise = do
      murl <- paste errors
      pure $ case murl of
        Just url -> firstSentence ++ " (additional errors suppressed, see " ++ url ++ ")"
        Nothing ->  firstSentence ++ " (additional errors suppressed)"
  where
    errors = T.unpack . T.strip . T.pack $ errStr

    numErrs = length . filter ("<interactive>" `isPrefixOf`) $ lines errors

    firstSentence
      | smallError `isPrefixOf` errors = drop (length smallError) errors
      | otherwise = unwords . words . filter (/='\n') $ go 0 errors where
          go :: Int -> String -> String
          go 0 ('•':cs) = go 1 cs
          go 0 (_:cs) = go 0 cs
          go 1 ('.':_) = ""
          go 1 ('‘':cs) = '‘' : go 2 cs
          go 2  ('’':cs) = '’' : go 1 cs
          go m (c:cs) = c : go m cs
          go _ "" = ""

    smallError = "<interactive>:1:1: error: " :: String

-- | Upload some text to sprunge and return the response body (the
-- URL).
paste :: String -> IO (Maybe String)
paste txt = upload `catch` handler where
  upload = do
    req <- W.parseRequest "http://sprunge.us"
    resp <- W.httpLbs =<< WM.formDataBody [WM.partBS "sprunge" (encodeUtf8 (T.pack txt))] req
    pure $ if W.getResponseStatusCode resp == 200
      then Just . filter (/='\n') . T.unpack . decodeUtf8 . toStrict $ W.getResponseBody resp
      else Nothing

  handler :: SomeException -> IO (Maybe a)
  handler = const $ pure Nothing
