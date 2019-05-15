{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Types
  ( Currency(..)
  , Category(..)
  , Balance(..)
  , Entry(..)
  , Row
  ) where

import qualified Data.Aeson   as Aeson
import qualified Date
import qualified GHC.Generics as Generics

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
  { acc             :: String
  , currency        :: Currency
  , transactionDate :: Maybe Date.Date
  , valueDate       :: Maybe Date.Date
  , balance         :: Balance
  , description     :: String
  , category        :: Category
  } deriving (Show, Generics.Generic, Aeson.ToJSON)
