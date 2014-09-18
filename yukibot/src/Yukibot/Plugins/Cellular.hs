{-# LANGUAGE OverloadedStrings #-}

-- |Plugin giving a command to evaluate elementary cellular automata.
module Yukibot.Plugins.Cellular (command) where

import Control.Comonad    (Comonad(..), (=>>))
import Control.Monad      (guard)
import Data.Bits          (setBit, testBit)
import Data.Maybe         (fromMaybe)
import Data.Monoid        ((<>))
import Data.Text          (pack, unpack)
import Data.Word          (Word8)
import Text.Read          (readMaybe)
import Network.IRC.Asakura.Commands (CommandDef(..))
import Network.IRC.Client (reply)

-- *Cellular automata

-- |We can represent the state of the cellular automata as an infinite
-- zip list, focussing at a single point.
data Zipper a = Z [a] a [a]

-- |We can move left
left :: Zipper a -> Zipper a
left (Z (l:ls) a rs) = Z ls l (a:rs)

-- |And right
right :: Zipper a -> Zipper a
right (Z ls a (r:rs)) = Z (a:ls) r rs

instance Functor Zipper where
  fmap f (Z ls a rs) = Z (map f ls) (f a) (map f rs)

instance Comonad Zipper where
  extract (Z _ a _) = a
  duplicate z = Z (tail $ iterate left z) z (tail $ iterate right z)

-- |Now we can apply a rule to the state, giving the new state as of
-- one step into the future.
rule :: Word8 -> Zipper Bool -> Zipper Bool
rule rnum state = state =>> rule'
  where
    -- Apply the rule at one point of the state
    rule' (Z (l:_) a (r:_)) = rulebits rnum !! (7 - (b2 . b1 . b0 $ 0))
      where
        b0 = if r then (`setBit` 0) else id
        b1 = if a then (`setBit` 1) else id
        b2 = if l then (`setBit` 2) else id

-- |Get the bits for a rule.
rulebits :: Word8 -> [Bool]
rulebits rnum = reverse [testBit rnum i | i <- [0..7]]

-- *Command

command :: CommandDef
command = CommandDef ["rule"] go
  where
    go [rnum] _ ev = return . reply ev $ case fmap rulebits . readMaybe $ unpack rnum of
      Just rbits -> "Rule " <> rnum <> " " <> tobits rbits <> " see " <> url
      Nothing -> "That's no rule!"

      where
        tobits bs = "[" <> pack (map (\x -> if x then '1' else '0') bs) <> "]"
        url = "http://www.wolframalpha.com/input/?i=rule+" <> rnum

    go [rnum, initial] state ev    = go [rnum, "1", initial] state ev
    go [rnum, steps, initial] _ ev = return . reply ev $ "You done goofed." `fromMaybe` stepped

      where
        stepped = do
          rulenum <- readMaybe $ unpack rnum
          steps'  <- readMaybe $ unpack steps
          state   <- readState $ unpack initial
          guard $ steps' <= 100
          return . pack . writeState $ iterate (rule rulenum) state !! steps'

    go _ _ _ = return $ return ()

-- |Read the initial state from a string.
readState :: String -> Maybe (Zipper Bool)
readState ss =
  if all (`elem` "01_") ss
  then Just . toZip . take 41 $ map (=='1') ss
  else Nothing

  where
    toZip (x:xs) = Z (repeat False) x (xs ++ repeat False)

-- |Write a final state to a string.
writeState :: Zipper Bool -> String
writeState = map (\i -> if i then '1' else '_') . toList (-20) 21
  where
    shift i z      = iterate (if i < 0 then left else right) z !! abs i
    toList i j     = take (j-i) . half . shift i
    half (Z _ a r) = a:r
