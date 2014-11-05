{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

{-# ANN module "HLint: ignore Use String" #-}

module Yukibot.Plugins.Brainfuck (command) where

import Control.Applicative ((<$), (<$>))
import Control.Lens hiding (noneOf)
import Control.Monad                (when)
import Control.Monad.IO.Class
import Control.Monad.Trans.State    (State, execState)
import qualified Data.DList as D
import Data.Maybe                   (fromMaybe)
import Data.Text                    (Text)
import qualified Data.Text as T
import Network.IRC.Asakura.Commands (CommandDef(..))
import Network.IRC.Client           (reply)
import System.Timeout
import Text.Parsec hiding (State)
import Text.Parsec.Text
import Yukibot.Utils.Types          (Zipper(..), left, right, focus)

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
data BFState = BFState { _tape   :: Zipper Int
                       , _input  :: [Char]
                       , _output :: D.DList Char
                       }
makeLenses ''BFState

defaultBFState :: BFState
defaultBFState = BFState { _tape = Z (repeat 0) 0 (repeat 0)
                         , _input = ""
                         , _output = D.empty
                         }

consumeElement :: BFElement -> State BFState ()
consumeElement (P z) = tape.focus += z
consumeElement L = tape %= left
consumeElement R = tape %= right
consumeElement I = tape.focus.enum <~ input %%= fromMaybe ('\0', "") . uncons
consumeElement O = do
  c <- use (tape.focus)
  output <>= D.singleton (toEnum c)
consumeElement (Loop l) = go
  where
    go = do
      c <- use (tape.focus)
      when (c /= 0) $ do
        mapM_ consumeElement l
        go

consumeProgram :: BFProgram -> State BFState ()
consumeProgram = mapM_ consumeElement

runProgram :: BFProgram -> [Char] -> D.DList Char
runProgram program i = _output $ execState (consumeProgram program)
                       defaultBFState {_input = i}

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

brainfuck :: Text -> Text -> Maybe Text
brainfuck program i = case parse parseProgram "" program of
  Left _ -> Nothing
  Right prog -> Just $ T.pack . D.toList . runProgram prog $ T.unpack i

command :: CommandDef
command = CommandDef { _verb   = ["bf"]
                     , _help   = "<program> - run the given brainfuck program."
                     , _action = go
                     }
  where
    go [program] ircs ev = go [program, ""] ircs ev
    go (program:is) _ ev = do
      o <- liftIO $ timeout 3000000 $ return $! fromMaybe "Sorry, I don't understand that brainfuck program! \
                                                          \Are brackets matched?" brainfuck program (T.unwords is)
      return . reply ev $ fromMaybe "Timed out." o
    go _ _ _ = return $ return ()
