module Date
  ( Date
  , readDate
  , format
  ) where

import qualified Data.Aeson   as Aeson
import qualified GHC.Generics as Generics
import qualified Text.Regex   as R

newtype Date =
  Date (String, String, String)
  deriving (Show) -- (YYYY, MM, DD)

instance Aeson.ToJSON Date where
  toJSON = Aeson.toJSON . format

-- formats a Date with "YYYY-MM-DD" style
format :: Date -> String
format (Date (y, m, d)) = y ++ "-" ++ m ++ "-" ++ d

listToDate :: [String] -> Maybe Date
listToDate [y, m, d] = Just $ Date (y, m, d)
listToDate _         = Nothing

readD :: R.Regex -> String -> Maybe Date
readD rx s = R.matchRegex rx s >>= listToDate

-- matches dates in the format "YYYYMMDD"
-- starting from YYYY = 2000
-- does not check if a months has enough days (e.g. 2019-02-31 would be valid)
dateRx = R.mkRegex "^(20[0-9]{2})(0[1-9]|1[0-2])(3[0-2]|[1-2][0-9]|0[1-9])$"

readDate :: String -> Maybe Date
readDate = readD dateRx

-- matches dates in the format "YYYY-MM-DD"
-- starting from YYYY = 2000
-- does not check if a months has enough days (e.g. 2019-02-31 would be valid)
formattedRx =
  R.mkRegex "^(20[0-9]{2})-(0[1-9]|1[0-2])-(3[0-2]|[1-2][0-9]|0[1-9])$"

readFormatedDate :: String -> Maybe Date
readFormatedDate = readD formattedRx
