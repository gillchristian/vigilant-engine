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
    , "gilles"
    , "belvedair"
    , "casino" -- we only would eat at a casino lol
    , "kfc"
    , "las sirenas"
    , "de beren"
    , "melly's cookies"
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
    , "doner"
    , "eethuis"
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
    , "ah de wolf"
    , "kruidvat"
    , "bakkerij"
    , "carrefour"
    , "supermarkt"
    , "supermercado"
    , "wijnhandel"
    , "plus"
    , "spar"
    , "notenshop"
    , "holland & barrett"
    ]

clothes :: R.Regex
clothes =
  mkUnionRegex
    [ "decathlon"
    , "vans"
    , "c&a"
    , "zara"
    , "hunkemoeller"
    , "bershka"
    , "h&m"
    , "h & m"
    ]

house :: R.Regex
house =
  mkUnionRegex ["blokker", "ikea", "abc store", "hema", "coolblue", "bol.com"]

salary :: R.Regex
salary = mkUnionRegex ["salary"]

reimbursement :: R.Regex
reimbursement = mkUnionRegex ["reimbursement", "refund"]

paypal :: R.Regex
paypal = mkUnionRegex ["paypal"]

gym :: R.Regex
gym = mkUnionRegex ["fit for free"]

transport :: R.Regex
transport = mkUnionRegex ["ns-", "ns ", "ov-chipkaart", "transavia"]

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
    , "ticketswap"
    , "eventbrite"
    ]

rent :: R.Regex
rent = mkUnionRegex ["rent", "xiaona liu"]

services :: R.Regex
services = mkUnionRegex ["evides", "tele2", "caiway"]

creditcard :: R.Regex
creditcard = mkUnionRegex ["creditcard"]

tax :: R.Regex
tax = mkUnionRegex ["iberia holding", "belasting"]

internal :: R.Regex
internal = mkUnionRegex ["cb gill desia cj"]

others :: R.Regex
others = mkUnionRegex ["easypayextra", "transferwise"]

cashRE :: R.Regex
cashRE = mkUnionRegex ["hoogstr", "banco", "betaalverzoek"]

volksuni :: R.Regex
volksuni = mkUnionRegex ["volksuni"]

incomingRE :: R.Regex
incomingRE = mkUnionRegex ["tikkie", "sepa overboeking", "ariel avendano"]

cash :: String -> Float -> Bool
cash desc amount = match cashRE desc && amount < 0

study :: String -> Float -> Bool
study desc amount = match volksuni desc && amount < (-10.0)

volksuniEat :: String -> Float -> Bool
volksuniEat desc amount = match volksuni desc && amount >= (-10.0)

incoming :: String -> Float -> Bool
incoming desc amount = match incomingRE desc && amount > 0

inferCategory :: Float -> String -> Category
inferCategory amount desc
  | match reimbursement desc = Reimbursement -- hast to be before eat out
  | match restaurants desc = EatOut
  | match food desc = EatOut
  | match salary desc = Salary
  | match internal desc = Internal
  | match services desc = Services
  | match rent desc = Rent
  | match creditcard desc = CreditCard
  | match paypal desc = PayPal
  | match transport desc = Transport
  | match clothes desc = Clothes
  | match markets desc = Groceries
  | match house desc = House
  | match bike desc = Bikes
  | match health desc = Health
  | match selfcare desc = SelfCare
  | match gym desc = Gym
  | match others desc = Others
  | match tax desc = Tax
  | match entertainment desc = Entertainment
  | cash desc amount = Cash
  | study desc amount = Study
  | volksuniEat desc amount = EatOut
  | incoming desc amount = Incoming
  | otherwise = Unknown
