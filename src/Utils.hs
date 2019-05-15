module Utils
  ( nth
  ) where

nth :: [a] -> Int -> Maybe a
nth xs n
  | n >= length xs = Nothing
  | n < 0 = Nothing
  | otherwise = Just (xs !! n)
