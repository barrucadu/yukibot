-- |Default load file for the Mueval plugin. Imports and definitions
-- can be added to this file to make them available to evaluated
-- expressions.
module L where

-- Use more generic versions of things
import Prelude hiding ( (.), id -- Control.Category
                      , foldr, foldr', foldl, foldl', foldr1, foldl1
                      , and, or, any, all, sum, product
                      , maximum, maximumBy, minimum, minimumBy
                      , concat, concatMap, elem, notElem, find -- Data.Foldable
                      , mapAccumL, mapAccumR, mapM, sequence, forM -- Data.Traversable
                      )

import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Monad hiding (mapM, sequence, forM) -- clash with Data.Traversable
import Control.Monad.Fix hiding (fix)
import Control.Monad.Zip

import Data.Bits
import Data.Bool
import Data.Char
import Data.Data
import Data.Dynamic
import Data.Either
import Data.Eq
import Data.Fixed
import Data.Foldable
import Data.Function hiding ((.), id)
import Data.Functor
import Data.Int
import Data.Ix
import Data.List hiding ( concat, concatMap
                        , foldl, foldl', foldl1, foldr, foldr1
                        , and, or, any, all, sum, product
                        , maximum, maximumBy, minimum, minimumBy
                        , elem, notElem, find -- clash with Data.Foldable
                        , mapAccumL, mapAccumR) -- clash with Data.Traversable
import Data.Maybe
import Data.Monoid
import Data.Ord
import Data.Ratio
import Data.String
import Data.Traversable
import Data.Tuple
import Data.Typeable
import Data.Unique
import Data.Version
import Data.Word

import ShowFun

import System.Random

-- |Show a string without quotes (good for generating input for other
-- bots!)
newtype NoQuotesString = NoQuotes { getString :: String }
instance Show NoQuotesString where
  show nqs = init . tail . show $ getString nqs

-- |Show a thing without quotes
noquote :: Show a => a -> NoQuotesString
noquote = NoQuotes . init . tail . show
