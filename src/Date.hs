module Date
  ( Date
  , readDate
  , format
  ) where

import qualified Text.Regex as R

-- TODO: implement ToJSON typeclass
type Date = (String, String, String) -- (YYYY, MM, DD)

-- matches dates in the format "YYYYMMDD"
-- starting from YYYY = 2000
-- does not check if a months has enough days (e.g. 2019-02-31 would be valid)
dateRx = R.mkRegex "^(20[0-9]{2})(0[1-9]|1[0-2])(3[0-2]|[1-2][0-9]|0[1-9])$"

-- matches dates in the format "YYYY-MM-DD"
-- starting from YYYY = 2000
-- does not check if a months has enough days (e.g. 2019-02-31 would be valid)
formattedRx = R.mkRegex "^(20[0-9]{2})-(0[1-9]|1[0-2])-(3[0-2]|[1-2][0-9]|0[1-9])$"

format :: Date -> String
format (y, m, d) = y ++ "-" ++ m ++ "-" ++ d

listToDate :: [String] -> Maybe Date
listToDate [y, m, d] = Just (y, m, d)
listToDate _         = Nothing

readD :: R.Regex -> String -> Maybe Date
readD rx s = R.matchRegex rx s >>= listToDate

readDate :: String -> Maybe Date
readDate = readD dateRx

readFormatedDate :: String -> Maybe Date
readFormatedDate = readD formattedRx
