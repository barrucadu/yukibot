-- |Utility functions
module Network.IRC.Bot.Utils where

import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Text (Text)
import Network.IRC.Client.Types (UnicodeEvent, IRC, Event(..), Message(Notice), Source(..))
import Network.IRC.Client (send)

import qualified Data.Text as T

-- |Group some tuples by fst
collect :: Ord a => [(a, b)] -> [(a, [b])]
collect = gather . sortBy (comparing fst)

-- |Kinda like 'group', but turns a [(a, b)] into a [(a, [b])]
gather :: Ord a => [(a, b)] -> [(a, [b])]
gather [] = []
gather ((a, b):xs) = (a, b : map snd ys) : gather zs where
  (ys, zs) = span ((==a) . fst) xs

-- |Send a message to the source of an event, as a NOTICE.
reply :: UnicodeEvent -> Text -> IRC ()
reply ev txt = case _source ev of
  Channel c _ -> mapM_ (send . Notice c . Right) $ T.lines txt
  User n      -> mapM_ (send . Notice n . Right) $ T.lines txt
  _           -> return ()
