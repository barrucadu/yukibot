{-# LANGUAGE OverloadedStrings #-}

module Yukibot.Plugins.Dedebtifier
  ( -- *Commands
    oweCmd
  , owedCmd
  , payCmd
  , listCmd
  ) where

import Control.Arrow (second)
import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO)
import Data.List (transpose, sort)
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import Data.Text (Text, pack, unpack)
import Database.MongoDB (Document, (=:), insert_)
import Network.IRC.Bot.Commands (CommandDef(..))
import Network.IRC.Client (UnicodeEvent, IRC, reply)
import Text.PrettyPrint.Boxes (render, hsep, left, vcat, text)
import Text.Read (readMaybe)
import Yukibot.Utils

import qualified Data.Text as T

-- * Currency

-- | Currency is represented as a number of pennies.
type Currency = Integer

-- | Split an amount of currency up into pounds and pence.
poundsAndPence :: Currency -> (Integer, Integer)
poundsAndPence money = money `divMod` 100

-- | Try to parse a string as containing some (positive) currency.
readCurrency :: Text -> Maybe Currency
readCurrency txt =
  if length pence /= 2
  then Nothing
  else (\a b -> 100*a + b) <$> readMaybe pounds <*> readMaybe pence

  where
    (pounds, pence) = (unpack . defZero) ^*^ second pad (breakOn' "." txt)

    defZero ""  = "00"
    defZero t   = t

    pad txt = if T.length txt == 1 then txt <> "0" else txt

-- | Display some currency as a string.
showCurrency :: Currency -> Text
showCurrency money = pack (show pounds) <> "." <> pack (pad $ show pence) where
  (pounds, pence) = poundsAndPence money
  pad ""  = "00"
  pad txt = if length txt == 1 then "0" <> txt else txt

-- * Commands

-- | Add a new debt from yourself to another person.
oweCmd :: Mongo -> CommandDef
oweCmd mongo = CommandDef
  { _verb   = ["owe"]
  , _help   = "owe <name> <amount> -- record that you owe money to someone."
  , _action = debtCmd $ addDebt mongo
  }

-- | Add a new debt from another person to yourself.
owedCmd :: Mongo -> CommandDef
owedCmd mongo = CommandDef
  { _verb   = ["owed"]
  , _help   = "owed <name> <amount> -- record that you are owed money by someone."
  , _action = debtCmd . flip $ addDebt mongo
  }

-- | Helper function for 'oweCmd' and 'owedCmd'
debtCmd :: Monad m => (Text -> Text -> Currency -> IRC ()) -> [Text] -> a -> UnicodeEvent -> m (IRC ())
debtCmd f [target, amount] _ ev = return $ case readCurrency amount of
  Just amount'
    | amount' <= 0 -> reply ev "Stop being silly."
    | otherwise -> do
      let me = sender' ev
      if me == target
      then reply ev "Stop being silly."
      else f me target amount'
  Nothing -> reply ev "Couldn't parse amount."
debtCmd _ _ _ ev = return $ reply ev "Expected two arguments."

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
      let me = sender' ev
      from <- queryMongo mongo (owes me) ["to"   =: (1::Int)]
      to   <- queryMongo mongo (owed me) ["from" =: (1::Int)]

      return $ case from ++ to of
        [] -> reply ev "You have no debts or credits."
        dc -> paste (toBoxes . sort $ listDebts dc) >>= reply ev

    listDebts = map $ \d -> [at' "from" "" d, at' "to" "" d, showCurrency $ at' "amount" 0 d]
    toBoxes rows = render $ hsep 2 left (map (vcat left . map (text . unpack))
                                             (transpose $ ["From", "To", "Amount"] : rows))

-- * Managing debts

-- | Add a new debt to the system, between the two people, for the
-- given amount. Then simplify their debts/credits.
addDebt :: MonadIO m => Mongo -> Text -> Text -> Currency -> m ()
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

-- | Simplify mutual debts involving the named person.
simplify :: MonadIO m => Mongo -> Text -> m ()
simplify mongo who = doMongo mongo $ \col -> do
  from <- queryMongo mongo (owes who) ["to"   =: (1::Int)]
  to   <- queryMongo mongo (owed who) ["from" =: (1::Int)]

  let mutuals = pairs (\a b -> txt "to" a == txt "from" b) (\a b -> txt "to" a `compare` txt "from" b) from to

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

  where
    txt fld = at' fld (""::Text)

-- * Utils

-- | Selector for all debts from a given person.
owes :: Text -> Document
owes from = sel (Just from) Nothing Nothing

-- | Selector for all debts to a given person.
owed :: Text -> Document
owed to = sel Nothing (Just to) Nothing

-- | Construct a debt record
debt :: Text -> Text -> Currency -> Document
debt from to amount = sel (Just from) (Just to) (Just amount)

-- | Construct a debt record/selector
sel :: Maybe Text -> Maybe Text -> Maybe Currency -> Document
sel from to amount = catMaybes
  [ "from"   ~: from
  , "to"     ~: to
  , "amount" ~: amount
  ]

  where
    l ~: v = (l =:) <$> v
