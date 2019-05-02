module Matchers
  ( inferCategory
  ) where

import qualified Data.Maybe        as M
import qualified Data.String.Utils as S
import qualified Text.Regex        as R
import           Types

match :: R.Regex -> String -> Bool
match x = M.isJust . R.matchRegex x

mkUnionRegex :: [String] -> R.Regex
mkUnionRegex = R.mkRegex . S.join "|"

restaurants :: R.Regex
restaurants =
  mkUnionRegex
    [ "dominos"
    , "happy italy"
    , "starbucks"
    , "simar"
    , "poke bowl"
    , "mcdona"
    , "bram ladage"
    , "deli"
    , "the corner"
    , "taqueria"
    , "juniors & binkys"
    , "marimina"
    , "sapporo"
    , "salsa shop"
    , "mcdonalds"
    , "bokaal"
    , "hmshost"
    , "sfeertje"
    ]

food :: R.Regex
food =
  mkUnionRegex
    [ "restaurant"
    , "food"
    , "fish"
    , "kip"
    , "delivery"
    , "friet"
    , "frite"
    , "coffee"
    , "cafe"
    , "burger"
    , "pizza"
    , "catering"
    , "durum"
    , "salad"
    , "tea"
    , "takeaway"
    , "pho"
    , "snackbar"
    ]

markets :: R.Regex
markets =
  mkUnionRegex
    [ "albert"
    , "heijn"
    , "dirk"
    , "libra"
    , "nightshop"
    , "ah to go"
    , "kruidvat"
    , "bakkerij"
    , "carrefour"
    , "supermarkt"
    , "supermercado"
    , "wijnhandel"
    , "plus"
    , "spar"
    ]

clothes :: R.Regex
clothes =
  mkUnionRegex ["decathlon", "vans", "c&a", "zara", "hunkemoeller", "bershka"]

house :: R.Regex
house = mkUnionRegex ["blokker", "ikea", "abc store", "hema", "coolblue"]

salary :: R.Regex
salary = mkUnionRegex ["salary"]

reimbursement :: R.Regex
reimbursement = mkUnionRegex ["reimbursement", "refund", "ariel avendano"]

paypal :: R.Regex
paypal = mkUnionRegex ["paypal"]

gym :: R.Regex
gym = mkUnionRegex ["fit for free"]

transport :: R.Regex
transport = mkUnionRegex ["ns-", "ns ", "ov-chipkaart"]

cashback :: R.Regex
cashback = mkUnionRegex ["sepa overboeking"] -- review this one

bike :: R.Regex
bike = mkUnionRegex ["bike", "fiet"]

health :: R.Regex
health = mkUnionRegex ["apotheek", "orthodont", "zorgverzekeraar"]

selfcare :: R.Regex
selfcare =
  mkUnionRegex ["la costa schiedam", "sila haar mode", "kapperskorting"]

entertainment :: R.Regex
entertainment =
  mkUnionRegex
    [ "tibbaa"
    , "cinema"
    , "pathe"
    , "netflix"
    , "racing poel"
    , "ticketmaster"
    , "ahoy"
    , "stichting worm"
    , "knvb"
    ]

rent :: R.Regex
rent = mkUnionRegex ["rent", "xiaona liu"]

services :: R.Regex
services = mkUnionRegex ["evides", "tele2", "caiway"]

creditcard :: R.Regex
creditcard = mkUnionRegex ["creditcard"]

tax :: R.Regex
tax = mkUnionRegex ["iberia holding", "belastingdienst"]

internal :: R.Regex
internal = mkUnionRegex ["cb gill desia cj"]

others :: R.Regex
others = mkUnionRegex ["easypayextra", "transferwise"]

cashRE :: R.Regex
cashRE = mkUnionRegex ["hoogstr", "banco", "betaalverzoek"]

volksuni :: R.Regex
volksuni = mkUnionRegex ["volksuni"]

incomingRE :: R.Regex
incomingRE = mkUnionRegex ["tikkie"]

cash :: String -> Float -> Bool
cash desc amount = match cashRE desc && amount < 0

study :: String -> Float -> Bool
study desc amount = match volksuni desc && amount < (-10.0)

volksuniEat :: String -> Float -> Bool
volksuniEat desc amount = match volksuni desc && amount >= (-10.0)

incoming :: String -> Float -> Bool
incoming desc amount = match incomingRE desc && amount > 0

descMatchers :: [(R.Regex, Category)]
descMatchers =
  [ (restaurants, EatOut)
  , (food, EatOut)
  , (salary, Salary)
  , (internal, Internal)
  , (services, Services)
  , (rent, Rent)
  , (creditcard, CreditCard)
  , (paypal, PayPal)
  , (transport, Transport)
  , (clothes, Clothes)
  , (markets, Groceries)
  , (house, House)
  , (bike, Bikes)
  , (health, Health)
  , (selfcare, SelfCare)
  , (gym, Gym)
  , (others, Others)
  , (tax, Tax)
  , (cashback, CashBack)
  , (entertainment, Entertainment)
  , (reimbursement, Reimbursement)
  ]

matchByDesc :: String -> (Bool, Category)
matchByDesc desc = foldr f (False, Unknown) descMatchers
  where
    f (_, _) (True, v) = (True, v)
    f (m, r) (_, _)    = (match m desc, r)

descAndAmountMatchers :: [(String -> Float -> Bool, Category)]
descAndAmountMatchers =
  [(cash, Cash), (study, Study), (volksuniEat, EatOut), (incoming, Incoming)]

matchByDescAndAmount :: String -> Float -> (Bool, Category)
matchByDescAndAmount desc amount =
  foldr f (False, Unknown) descAndAmountMatchers
  where
    f (_, _) (True, v) = (True, v)
    f (m, r) (_, _)    = (m desc amount, r)

inferCategory :: Float -> String -> Category
inferCategory amount desc
  | ma = ra
  | mb = rb
  | otherwise = Unknown
  where
    (ma, ra) = matchByDesc desc
    (mb, rb) = matchByDescAndAmount desc amount
