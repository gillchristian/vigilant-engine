module Matchers
  ( match
  , restaurants
  , food
  , markets
  , clothes
  , house
  , salary
  , reimbursement
  , paypal
  , gym
  , transport
  , cash
  , cashback
  , bike
  , health
  , selfcare
  , entertainment
  , rent
  , services
  , creditcard
  , incoming
  , volksuni
  , tax
  , internal
  , others
  ) where

import qualified Data.Maybe        as M
import qualified Data.String.Utils as S
import qualified Text.Regex        as R

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

cash :: R.Regex
cash = mkUnionRegex ["hoogstr", "banco", "betaalverzoek"]

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

volksuni :: R.Regex
volksuni = mkUnionRegex ["volksuni"]

incoming :: R.Regex
incoming = mkUnionRegex ["tikkie"]

tax :: R.Regex
tax = mkUnionRegex ["iberia holding", "belastingdienst"]

internal :: R.Regex
internal = mkUnionRegex ["cb gill desia cj"]

others :: R.Regex
others = mkUnionRegex ["easypayextra", "transferwise"]
