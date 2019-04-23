{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Parsing
  ( entries
  ) where

import qualified Data.Aeson        as Aeson
import qualified Data.Char         as C
import qualified Data.Maybe        as M
import qualified Data.String.Utils as S
import qualified GHC.Generics      as Generics
import qualified Matchers          as Mx
import qualified Text.Read         as Read

type Row = (String, String, String, String, String, String, String, String)

data Currency
  = EUR
  | Other
  deriving (Eq, Show, Generics.Generic, Aeson.ToJSON)

data Category
  = Groceries
  | EatOut
  | Transport
  | Internal
  | Rent
  | Services
  | Salary
  | Reimbursement
  | Incoming
  | Cash -- taken from ATM
  | CashBack
  | Study
  | Health
  | SelfCare
  | Tax
  | House
  | Gym
  | Entertainment
  | Clothes
  | Bikes
  | CreditCard
  | PayPal
  | Others
  | Unknown
  deriving (Eq, Show, Generics.Generic, Aeson.ToJSON)

data Balance = Balance
  { before :: Float
  , after  :: Float
  , debit  :: Float
  } deriving (Show, Generics.Generic, Aeson.ToJSON)

data Entry = Entry
  { acc         :: String
  , currency    :: Currency
  , date1       :: String
  , date2       :: String
  , balance     :: Balance
  , description :: String
  , category    :: Category
  } deriving (Show, Generics.Generic, Aeson.ToJSON)

readCurrency :: String -> Currency
readCurrency "EUR" = EUR
readCurrency _     = Other

readAmount :: String -> Float
readAmount ('-':s) = -(readAmount s)
readAmount s       = M.fromMaybe 0 $ Read.readMaybe $ S.replace "," "." s

row :: [String] -> Maybe Row
row [acc, cur, d1, before, after, d2, debit, desc] =
  Just (acc, cur, d1, before, after, d2, debit, desc)
row _ = Nothing

inferCategory :: Float -> String -> Category
inferCategory amount desc
  | Mx.match Mx.restaurants desc = EatOut
  | Mx.match Mx.food desc = EatOut
  | Mx.match Mx.salary desc = Salary
  | Mx.match Mx.internal desc = Internal
  | Mx.match Mx.services desc = Services
  | Mx.match Mx.rent desc = Rent
  | Mx.match Mx.creditcard desc = CreditCard
  | Mx.match Mx.paypal desc = PayPal
  | Mx.match Mx.transport desc = Transport
  | Mx.match Mx.clothes desc = Clothes
  | Mx.match Mx.markets desc = Groceries
  | Mx.match Mx.cash desc && amount < 0 = Cash
  | Mx.match Mx.house desc = House
  | Mx.match Mx.bike desc = Bikes
  | Mx.match Mx.health desc = Health
  | Mx.match Mx.selfcare desc = SelfCare
  | Mx.match Mx.entertainment desc = Entertainment
  | Mx.match Mx.volksuni desc && amount < (-10.0) = Study
  | Mx.match Mx.volksuni desc && amount >= (-10.0) = EatOut
  | Mx.match Mx.reimbursement desc = Reimbursement
  | Mx.match Mx.gym desc = Gym
  | Mx.match Mx.others desc = Others
  | Mx.match Mx.tax desc = Tax
  | Mx.match Mx.incoming desc && amount > 0.0 = Incoming
  | Mx.match Mx.cashback desc = CashBack
  | otherwise = Unknown

{-
const strToDate = (str: string): Date => {
  const [, y, m, d] = /([0-9]{4})([0-9]{2})([0-9]{2})/.exec(str)
  return new Date(parseInt(y, 10), parseInt(m, 10) - 1, parseInt(d, 10) + 1)
}
-}
entryOfRow :: Row -> Entry
entryOfRow (acc, cur, d1, before, after, d2, debit, desc) =
  Entry
    { acc = acc
    , currency = readCurrency cur
    , date1 = d1
    , date2 = d2
    , balance = balance'
    , description = desc
    , category = inferCategory debit' $ map C.toLower desc
    }
  where
    debit' = readAmount debit
    after' = readAmount after
    before' = readAmount before
    balance' = Balance {before = before', after = after', debit = debit'}

entries :: String -> [Entry]
entries =
  map entryOfRow . M.mapMaybe (row . S.split "\t" . S.strip) . lines . S.strip
