{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Yukibot.Plugins.Brainfuck (brainfuck, command, command8bit) where

import Control.Lens hiding (noneOf)
import Control.Monad (when)
import Control.Monad.IO.Class
import Control.Monad.Trans.State (State, execState)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Network.IRC.Bot.Commands (CommandDef(..))
import Network.IRC.Bot.Events (reply)
import Network.IRC.Bot.Types (StatefulBot)
import Network.IRC.Client (UnicodeEvent, StatefulIRC, IRCState)
import System.Timeout
import Text.Parsec hiding (State)
import Text.Parsec.Text

import Yukibot.Utils.Types

import qualified Data.DList as D
import qualified Data.Text  as T

{-# ANN module ("HLint: ignore Use String" :: String) #-}

-- |Data type representing a single brainfuck element, that is, a single command
-- or a loop.
--
-- Names are abbreviated for brevity and to avoid clashing with Prelude. (I'm
-- looking at you, Either....)
--
-- Multiple consecutive +s and -s are combined.
data BFElement = L | R | P Int | I | O | Loop [BFElement]

-- |A brainfuck program is just a list of brainfuck elements.
type BFProgram = [BFElement]

-- |The state of a brainfuck program is its tape, its input, and its output.
data BFState = BFState
  { _tape   :: Zipper Int
  , _input  :: [Char]
  , _output :: D.DList Char
  }
makeLenses ''BFState

defaultBFState :: BFState
defaultBFState = BFState
  { _tape = Z (repeat 0) 0 (repeat 0)
  , _input = ""
  , _output = D.empty
  }

consumeElement :: Bool -> BFElement -> State BFState ()
consumeElement is8bit (P z) = tape.focus %= (\x -> let x' = x + z in if is8bit then x' `mod` 256 else x')
consumeElement _ L = tape %= left
consumeElement _ R = tape %= right
consumeElement _ I = tape.focus.enum <~ input %%= fromMaybe ('\0', "") . Control.Lens.uncons
consumeElement _ O = do
  c <- use (tape.focus)
  output <>= D.singleton (toEnum c)
consumeElement is8bit (Loop l) = go where
  go = do
    c <- use (tape.focus)
    when (c /= 0) $ do
      mapM_ (consumeElement is8bit) l
      go

consumeProgram :: Bool -> BFProgram -> State BFState ()
consumeProgram is8bit = mapM_ (consumeElement is8bit)

runProgram :: Bool -> BFProgram -> [Char] -> D.DList Char
runProgram is8bit program i = _output $
  execState (consumeProgram is8bit program) defaultBFState {_input = i}

parseProgram :: Parser BFProgram
parseProgram = (>>) comment $ many $ choice [
  parsePM,
  parseLoop,
  L <$ char '<',
  R <$ char '>',
  I <$ char ',',
  O <$ char '.'] >>= \x -> x <$ comment
  where
    parsePM = fmap (P . sum) $ many1 $ choice [
      1    <$ char '+',
      (-1) <$ char '-']
    parseLoop = Loop <$> between (char '[') (char ']') parseProgram
    comment = skipMany (noneOf "+-<>,.[]")

brainfuck :: Bool -> Text -> Text -> Maybe Text
brainfuck is8bit program i = case parse parseProgram "" program of
  Left _ -> Nothing
  Right prog -> Just $ T.pack . D.toList . runProgram is8bit prog $ T.unpack i

command :: CommandDef s
command = CommandDef
  { _verb   = ["bf"]
  , _help   = "<program> - run the given brainfuck program."
  , _action = commandAction False
  }

command8bit :: CommandDef s
command8bit = CommandDef
  { _verb   = ["bf8"]
  , _help   = "<program> - run the given brainfuck program with 8-bit cells."
  , _action = commandAction True
  }

commandAction :: Bool -> [Text] -> IRCState s -> UnicodeEvent -> StatefulBot s (StatefulIRC s ())
commandAction is8bit [program] ircs ev = commandAction is8bit [program, ""] ircs ev
commandAction is8bit (program:is) _ ev = do
  o <- liftIO $ timeout 3000000 $ return $! fromMaybe "Sorry, I don't understand that brainfuck program! \
                                                      \Are brackets matched?" $ brainfuck is8bit program (T.unwords is)
  return . reply ev $ fromMaybe "Timed out." o
commandAction _ _ _ _ = return $ return ()
