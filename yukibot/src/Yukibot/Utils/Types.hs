module Yukibot.Utils.Types where

import Control.Comonad (Comonad(..))
import Control.Lens

-- |An infinite zip list, focussing at a single point.
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

-- |A lens operating over the focus of a `Zipper`
focus :: Lens' (Zipper a) a
focus f (Z ls a rs) = fmap (\b -> Z ls b rs) (f a)
