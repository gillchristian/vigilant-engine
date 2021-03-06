module Parsing
  ( parseEntries
  ) where

import qualified Data.Char         as C
import qualified Data.Maybe        as M
import qualified Data.String.Utils as S
import qualified Date
import qualified Matchers          as Mx
import qualified Text.Read         as Read
import           Types

readCurrency :: String -> Currency
readCurrency "EUR" = EUR
readCurrency _     = Other

readAmount :: String -> Float
readAmount ('-':s) = -(readAmount s)
readAmount s       = M.fromMaybe 0 $ Read.readMaybe $ S.replace "," "." s

row :: [String] -> Maybe Row
row [a, b, c, d, e, f, g, h] = Just (a, b, c, d, e, f, g, h)
row _                        = Nothing

entryOfRow :: Row -> Entry
entryOfRow (account, cur, d1, before', after', d2, deb, desc) =
  Entry
    { acc = account
    , currency = readCurrency cur
    , transactionDate = Date.readDate d1
    , valueDate = Date.readDate d2
    , balance = balance'
    , description = Nothing
    -- , description = Mx.parseDesc desc' category'
    , originalDesc = desc
    , category = category'
    }
  where
    debit' = readAmount deb
    after'' = readAmount after'
    before'' = readAmount before'
    balance' = Balance {before = before'', after = after'', debit = debit'}
    desc' = map C.toLower desc
    category' = Mx.inferCategory debit' desc'

parseEntries :: String -> [Entry]
parseEntries =
  map entryOfRow . M.mapMaybe (row . S.split "\t" . S.strip) . lines . S.strip
