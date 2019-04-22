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

import qualified Data.Aeson        as Aeson
import qualified Data.Char         as C
import qualified Data.Maybe        as M
import qualified Data.String.Utils as S
import qualified GHC.Generics      as Generics
import qualified Text.Read         as Read
import qualified Text.Regex        as R

match :: R.Regex -> String -> Bool
match x = M.isJust . R.matchRegex x

mkUnionRegex :: [String] -> R.Regex
mkUnionRegex = R.mkRegex . S.join "|"

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

clothes =
  mkUnionRegex ["decathlon", "vans", "c&a", "zara", "hunkemoeller", "bershka"]

house = mkUnionRegex ["blokker", "ikea", "abc store", "hema", "coolblue"]

salary = mkUnionRegex ["salary"]

reimbursement = mkUnionRegex ["reimbursement", "refund", "ariel avendano"]

paypal = mkUnionRegex ["paypal"]

gym = mkUnionRegex ["fit for free"]

transport = mkUnionRegex ["ns-", "ns ", "ov-chipkaart"]

cash = mkUnionRegex ["hoogstr", "banco", "betaalverzoek"]

cashback = mkUnionRegex ["sepa overboeking"] -- review this one

bike = mkUnionRegex ["bike", "fiet"]

health = mkUnionRegex ["apotheek", "orthodont", "zorgverzekeraar"]

selfcare =
  mkUnionRegex ["la costa schiedam", "sila haar mode", "kapperskorting"]

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

rent = mkUnionRegex ["rent", "xiaona liu"]

services = mkUnionRegex ["evides", "tele2", "caiway"]

creditcard = mkUnionRegex ["creditcard"]

volksuni = mkUnionRegex ["volksuni"]

incoming = mkUnionRegex ["tikkie"]

tax = mkUnionRegex ["iberia holding", "belastingdienst"]

internal = mkUnionRegex ["cb gill desia cj"]

others = mkUnionRegex ["easypayextra", "transferwise"]
