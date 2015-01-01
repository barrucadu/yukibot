{-# LANGUAGE OverloadedStrings #-}

module Yukibot.Plugins.Dedebtifier
  ( -- *Commands
    oweCmd
  , owedCmd
  , payCmd
  , listCmd
  ) where

import Control.Applicative ((<$>), (<*>))
import Control.Arrow ((***))
import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO)
import Data.List (transpose, sort)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text, breakOn, stripPrefix, pack, unpack)
import Database.MongoDB (Document, (=:), insert_)
import Network.IRC.Asakura.Commands (CommandDef(..))
import Network.IRC.Client (Event(_source), Source(..), reply)
import Text.PrettyPrint.Boxes (render, hsep, left, vcat, text)
import Text.Read (readMaybe)
import Yukibot.Utils

-- | Add a new debt from yourself to another person.
oweCmd :: Mongo -> CommandDef
oweCmd mongo = CommandDef
  { _verb   = ["owe"]
  , _help   = "owe <name> <amount> -- record that you owe money to someone."
  , _action = go
  }

  where
    go [to, amount] _ ev = return $ case readCur amount of
      Just amount' -> do
        let User from = _source ev
        addDebt mongo from to amount'
      Nothing -> reply ev "Couldn't parse amount."
    go _ _ ev = return $ reply ev "Expected two arguments."

-- | Add a new debt from another person to yourself.
owedCmd :: Mongo -> CommandDef
owedCmd mongo = CommandDef
  { _verb   = ["owed"]
  , _help   = "owed <name> <amount> -- record that you are owed money by someone."
  , _action = go
  }

  where
    go [from, amount] _ ev = return $ case readCur amount of
      Just amount' -> do
        let User to = _source ev
        addDebt mongo from to amount'
      Nothing -> reply ev "Couldn't parse amount."
    go _ _ ev = return $ reply ev "Expected two arguments."

-- | Pay someone money.
payCmd :: Mongo -> CommandDef
payCmd mongo = CommandDef
  { _verb   = ["pay"]
  , _help   = "pay <name> <amount> -- pay someone some money, this is a synonym for 'owed'."
  , _action = _action $ owedCmd mongo
  }

-- | List (simplified) debts in the system.
listCmd :: Mongo -> CommandDef
listCmd mongo = CommandDef
  { _verb   = ["debts"]
  , _help   = "debts -- list all your debts and credits."
  , _action = go
  }

  where
    go _ _ ev = do
      let User nick = _source ev
      from <- queryMongo mongo (sel (Just nick) Nothing Nothing) ["to"   =: (1::Int)]
      to   <- queryMongo mongo (sel Nothing (Just nick) Nothing) ["from" =: (1::Int)]

      let output = toBoxes . sort . listDebts $ from ++ to

      return $ case output of
        "" -> reply ev "You have no debts or credits."
        _ | '\n' `elem` output -> paste output >>= reply ev
          | otherwise -> reply ev $ pack output

    listDebts = map $ \d -> [at' "from" "" d, at' "to" "" d, showCur $ at' "amount" 0 d]
    toBoxes rows = render $ hsep 2 left (map (vcat left . map (text . unpack)) (transpose rows))

-- * Managing debts

-- | Add a new debt to the system, between the two people, for the
-- given amount. Then simplify their debts/credits.
addDebt :: MonadIO m => Mongo -> Text -> Text -> Integer -> m ()
addDebt mongo from to amount = do
  doMongo mongo $ \c -> do
    let qS = sel (Just from) (Just to) Nothing

    extant <- queryMongo mongo qS []
    deleteMongo mongo qS

    let amount' = amount + case extant of
          [e] -> at' "amount" 0 e
          _  -> 0

    insert_ c $ debt from to amount'

  simplify mongo from
  simplify mongo to

-- | Construct a debt record
debt :: Text -> Text -> Integer -> Document
debt from to amount = sel (Just from) (Just to) (Just amount)

-- | Construct a debt record/selector
sel :: Maybe Text -> Maybe Text -> Maybe Integer -> Document
sel from to amount = catMaybes
  [ "from"   ~: from
  , "to"     ~: to
  , "amount" ~: amount]

  where
    l ~: v = (l =:) <$> v

-- | Simplify debts/credits in the system involving the named person.
simplify :: MonadIO m => Mongo -> Text -> m ()
simplify mongo who = doMongo mongo credits
  where
    -- | Simplify mutual debts
    credits col = do
      owes <- queryMongo mongo owesS ["to"   =: (1::Int)]
      cred <- queryMongo mongo owedS ["from" =: (1::Int)]

      let mutuals = pairs (\a b -> txt "to" a == txt "from" b) (\a b -> txt "to" a `compare` txt "from" b) owes cred

      forM_ mutuals $ \(d,c) -> do
        let d' = at' "amount" 0 d
        let c' = at' "amount" 0 c

        let other = at' "to" "" d

        deleteMongo mongo d
        deleteMongo mongo c

        case () of
          _ | c' > d'   -> insert_ col $ debt other who (c' - d')
            | d' > c'   -> insert_ col $ debt who other (d' - c')
            | otherwise -> return ()

    owesS = sel (Just who) Nothing Nothing
    owedS = sel Nothing (Just who) Nothing

    txt fld = at' fld (""::Text)

-- * Utils

-- | Try to read a currency value from a Text.
readCur :: Text -> Maybe Integer
readCur txt = (\a b -> 100*a + b) <$> readMaybe pounds <*> readMaybe pence where
  (pounds, pence) = ((unpack . defZero) *** (unpack . defZero')) $ breakOn "." txt

  defZero ""  = "0"
  defZero t   = t

  defZero' "" = "0"
  defZero' t = fromMaybe "0" $ stripPrefix "." t

-- | Display an Integer as a number of pence
showCur :: Integer -> Text
showCur cur = pack (show pounds) <> "." <> pack (show pence) where
  (pounds, pence) = cur `divMod` 100

-- | Group two sorted lists into pairs, dropping the lesser element
-- when pairs do not match.
pairs :: (a -> b -> Bool) -> (a -> b -> Ordering) -> [a] -> [b] -> [(a,b)]
pairs _ _ _ [] = []
pairs _ _ [] _ = []
pairs eq cmp (a:as) (b:bs)
  | a `eq`  b      = (a,b) : pairs eq cmp as bs
  | a `cmp` b == GT = pairs eq cmp (a:as) bs
  | otherwise      = pairs eq cmp as (b:bs)
