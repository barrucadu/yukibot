{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

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
import Data.Function (on)
import Data.List (transpose)
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
  , _help   = "pay <name> <amount> -- pay someone some money, this may put you in credit which will be redemmed automatically against future debts."
  , _action = go
  }

  where
    go [to, amount] _ ev = return $ case readCur amount of
      Just amount' -> do
        let User from = _source ev
        addCredit mongo from to amount'
      Nothing -> reply ev "Couldn't parse amount."
    go _ _ ev = return $ reply ev "Expected two arguments."

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
      creditFrom <- queryMongo mongo (sel (Just "credit") (Just nick) Nothing Nothing) ["to"   =: (1::Int)]
      creditTo   <- queryMongo mongo (sel (Just "credit") Nothing (Just nick) Nothing) ["from" =: (1::Int)]
      debtFrom   <- queryMongo mongo (sel (Just "debt")   (Just nick) Nothing Nothing) ["to"   =: (1::Int)]
      debtTo     <- queryMongo mongo (sel (Just "debt")   Nothing (Just nick) Nothing) ["from" =: (1::Int)]

      let output = toBoxes . sort . listDebts $ creditFrom ++ creditTo ++ debtFrom ++ debtTo

      return $ case output of
        "" -> reply ev "You have no debts or credits."
        _ | '\n' `elem` output -> paste output >>= reply ev
          | otherwise -> reply ev $ pack output

    listDebts = map $ \d -> [at' "from" "" d, at' "to" "" d, at' "type" "debt" d, showCur $ at' "amount" 0 d]
    toBoxes rows = render $ hsep 2 left (map (vcat left . map (text . unpack)) (transpose rows))

-- * Managing debts

-- | Add a new debt to the system, between the two people, for the
-- given amount. Then simplify their debts/credits.
addDebt :: MonadIO m => Mongo -> Text -> Text -> Integer -> m ()
addDebt = flip add "debt"

-- | Add a new credit to the system, between the two people, for the
-- given amount. Then simplify their debts/credits.
addCredit :: MonadIO m => Mongo -> Text -> Text -> Integer -> m ()
addCredit = flip add "credit"

-- | Add a new debt/credit, adding to what was already there (if any).
add :: MonadIO m => Mongo -> Text -> Text -> Text -> Integer -> m ()
add mongo type_ from to amount = do
  doMongo mongo $ \c -> do
    let qS = sel (Just type_) (Just from) (Just to) Nothing

    extant <- queryMongo mongo qS []
    deleteMongo mongo qS

    let amount' = amount + case extant of
          [e] -> at' "amount" 0 e
          _  -> 0

    insert_ c $ mk type_ from to amount'

  simplify mongo from
  simplify mongo to

-- | Construct a debt record
debt :: Text -> Text -> Integer -> Document
debt = mk "debt"

-- | Construct a credit record"
credit :: Text -> Text -> Integer -> Document
credit = mk "credit"

-- | Construct a debt/credit record
mk :: Text -> Text -> Text -> Integer -> Document
mk type_ from to amount = sel (Just type_) (Just from) (Just to) (Just amount)

-- | Construct a debt/credit record/selector
sel :: Maybe Text -> Maybe Text -> Maybe Text -> Maybe Integer -> Document
sel type_ from to amount = catMaybes
  [ "type"   ~: type_
  , "from"   ~: from
  , "to"     ~: to
  , "amount" ~: amount]

  where
    l ~: v = (l =:) <$> v

-- | Simplify debts/credits in the system involving the named person.
simplify :: MonadIO m => Mongo -> Text -> m ()
simplify mongo who = doMongo mongo credits
  where
    -- | Apply credits against debts
    credits col = do
      owes <- queryMongo mongo owesS   ["to" =: (1::Int)]
      cred <- queryMongo mongo creditS ["to" =: (1::Int)]

      let mutuals = pairs' (txt "to") owes cred

      forM_ mutuals $ \(d,c) -> do
        let d' = at' "amount" 0 d
        let c' = at' "amount" 0 c

        let to = at' "to" "" d

        deleteMongo mongo d
        deleteMongo mongo c

        case () of
          _ | c' > d'   -> insert_ col $ credit who to (c' - d')
            | d' > c'   -> insert_ col $ debt   who to (d' - c')
            | otherwise -> return ()

    owesS   = sel (Just "debt")   (Just who) Nothing Nothing
    creditS = sel (Just "credit") (Just who) Nothing Nothing

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

-- | Like pairs, but use (==) `on` and compare `on`.
pairs' :: (Eq b, Ord b) => (a -> b) -> [a] -> [a] -> [(a,a)]
pairs' f = pairs ((==) `on` f) (compare `on` f)
