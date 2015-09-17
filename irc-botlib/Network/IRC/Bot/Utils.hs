-- |Utility functions
module Network.IRC.Bot.Utils where

import Data.List (sortBy)
import Data.Ord (comparing)

-- |Group some tuples by fst
collect :: Ord a => [(a, b)] -> [(a, [b])]
collect = gather . sortBy (comparing fst)

-- |Kinda like 'group', but turns a [(a, b)] into a [(a, [b])]
gather :: Ord a => [(a, b)] -> [(a, [b])]
gather [] = []
gather ((a, b):xs) = (a, b : map snd ys) : gather zs where
  (ys, zs) = span ((==a) . fst) xs

