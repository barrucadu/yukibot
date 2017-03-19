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
--     * "check", checks a property.
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

import Control.Arrow (second)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, takeMVar, putMVar)
import Control.Exception (evaluate)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import qualified Data.HashMap.Strict as H
import Data.List (intercalate, isInfixOf, isPrefixOf)
import Data.Maybe (catMaybes, fromMaybe, isNothing)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import System.Environment (getEnvironment)
import System.IO (hGetContents, hPutStr, hClose)
import System.Process (CreateProcess(..), StdStream(CreatePipe), createProcess, proc, waitForProcess)
import System.Random (randomIO)

import Yukibot.Core
import Yukibot.Utils

muevalPlugin :: Table -> Either error Plugin
muevalPlugin cfg = Right Plugin
  { pluginHelp = "bringing Haskell evaluation to a channel near you"
  , pluginMonitors = H.fromList [("inline", inlineMonitor cfg)]
  , pluginCommands = H.fromList
    [ ("eval",  evalCommand cfg)
    , ("check", checkCommand cfg)
    , ("type",  typeCommand cfg)
    , ("kind",  kindCommand cfg)
    ]
  }

-- | Automatically interpret lines starting with '> ', ':t ', and ':k '.
inlineMonitor :: Table -> Monitor
inlineMonitor cfg = Monitor
  { monitorHelp = "automatically interpret lines starting with '> ', ':t ', and ':k '"
  , monitorAction = \ev -> case eventMessage ev of
      msg | "> "  `T.isPrefixOf` msg -> muevalOrGHCi False cfg Nothing     (T.strip $ T.drop 1 msg)
          | ":t " `T.isPrefixOf` msg -> muevalOrGHCi False cfg (Just ":t") (T.strip $ T.drop 2 msg)
          | ":k " `T.isPrefixOf` msg -> muevalOrGHCi False cfg (Just ":k") (T.strip $ T.drop 2 msg)
      _ -> pure ()
  }

-- | Evaluate Haskell expressions.
evalCommand :: Table -> Command
evalCommand cfg = Command
  { commandHelp = "\"haskell\": evaluate Haskell expressions"
  , commandAction = const (muevalOrGHCi False cfg Nothing . T.unwords)
  }

-- | Check Haskell properties.
checkCommand :: Table -> Command
checkCommand cfg = Command
  { commandHelp = "\"property\": check Haskell properties"
  , commandAction = const (muevalOrGHCi False cfg Nothing . T.unwords . (["check", "$"]++))
  }

-- | Get the type of a Haskell expression.
typeCommand :: Table -> Command
typeCommand cfg = Command
  { commandHelp = "\"haskell\": get the type of a Haskell expression"
  , commandAction = const (muevalOrGHCi False cfg (Just ":t") . T.unwords)
  }

-- | Get the kind of a Haskell type.
kindCommand :: Table -> Command
kindCommand cfg = Command
  { commandHelp = "\"haskell\": get the kind of a Haskell type"
  , commandAction = const (muevalOrGHCi False cfg (Just ":k") . T.unwords)
  }

-------------------------------------------------------------------------------
-- Evaluation

-- | Evaluate an expression, or get its type or kind.
muevalOrGHCi :: Bool -> Table -> Maybe String -> Text -> BackendM ()
muevalOrGHCi seeded cfg mcmd expr = do
  expr' <- liftIO $ if seeded || mcmd == Just ":k" then pure (T.unpack expr) else do
    seed <- randomIO :: IO Int
    pure ("let seed = " ++ show seed ++ " in " ++ T.unpack expr)

  (out, err) <- liftIO $ maybe (mueval cfg) (queryGHCi cfg) mcmd expr'

  let isMueval = isNothing mcmd

  case formatErrors err of
    Just (short, full)
      -- If we get a horrible type error froom mueval, use queryGHCi
      -- to get a nicer one (or show the type, as it might be a
      -- Typeable thing).
      | isMueval && "arising from a use of ‘show_M" `isInfixOf` short -> muevalOrGHCi True cfg (Just ":t") (T.pack expr')
      | otherwise -> case full of
          Just errors -> do
            murl <- liftIO (paste errors)
            quickReply . T.pack $ case murl of
              Just url -> short ++ " (additional errors suppressed, see " ++ url ++ ")"
              Nothing ->  short ++ " (additional errors suppressed)"
          Nothing -> quickReply (T.pack short)
    Nothing
      | isMueval  -> quickReply . T.pack $ formatExpr       out
      | otherwise -> quickReply . T.pack $ formatTypeOrKind out

-- | Evaluate an expression.
mueval :: Table -> String -> IO (String, String)
mueval cfg expr = do
  env <- (("LC_ALL", "C"):) <$> getEnvironment
  (out, err) <- runProcess binary (opts ++ baseOpts expr) (Just env) ""

  -- mueval MOSTLY doesn't use stderr because it is special.
  pure $ case () of
    _ | "error:"    `isPrefixOf` out -> ("", out)
      | "<hint>:1:" `isPrefixOf` out -> ("", out)
      | not (null err) -> (out, "error: " <> err)
      | otherwise -> (out, "")

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
queryGHCi :: Table -> String -> String -> IO (String, String)
queryGHCi cfg cmd expr = do
  env <- (("LC_ALL", "C"):) <$> getEnvironment
  runProcess binary (opts ++ baseOpts) (Just env) input

  where
    input = case getString "load-file" cfg of
      Just loadFile | not (T.null loadFile) ->
        ":load " <> T.unpack loadFile <> "\n:m *L\n" <> cmd <> " " <> expr
      _ -> cmd <> " " <> expr

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
runProcess :: FilePath -> [String] -> Maybe [(String, String)] -> String -> IO (String, String)
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
  _ <- waitForProcess pid

  pure (out, err)

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
  go [line] =
    let (_, tK) = second (T.unpack . T.strip . T.drop 4) $ T.breakOn " :: " (T.pack line)
    in if " :: " `isInfixOf` tK
       -- Strip out the extra "::" in the result of things like ":t foo :: a"
       then go [tK]
       else "<" ++ tK ++ ">"
  go _ = ""

-- | Format some errors: return the first sentence of the first error
-- message, and some extra output to paste.
formatErrors :: String -> Maybe (String, Maybe String)
formatErrors errs0
  | null errors = Nothing
  | length errors == 1 = Just (firstSentence, Nothing)
  | otherwise = Just (firstSentence, Just (intercalate sep errors))

  where
    -- List of all errors.
    errors = filter (not . null) . go [] $ lines errs0 where
      go soFar (l:ls)
        | "error:" `isInfixOf` l =
            let (_, l') = second (T.unpack . T.strip . T.drop 6) $ T.breakOn "error:" (T.pack l)
                rest = if isEmpty l' then go [] ls else go [l'] ls
            in unlines (reverse soFar) : rest
        | isEmpty l = go soFar ls
        | otherwise = go (l:soFar) ls
      go soFar [] = [unlines (reverse soFar)]

    -- First sentence of the first error
    firstSentence =
      let -- Sentence enders
          go _    False ('.':_) = ""
          go True False ('•':_) = ""
          go _ _ "" = ""
          -- Quoting (disabled sentence enders)
          go b False ('‘':cs) = '‘' : go b True cs
          go b True  ('’':cs) = '’' : go b False cs
          -- First bullet (seeing a second bullet is a sentence ender)
          go False m ('•':cs) = '•' : go True m cs
          -- Anything else
          go b m (c:cs) = c : go b m cs

          sentence = unwords . words $ go False False (head errors)
      in if "• " `isPrefixOf` sentence then drop 2 sentence else sentence

    sep = "\n-------------------------------------------------\n\n"

    isEmpty = (=="") . T.strip . T.pack
