module Main where

import qualified Data.Aeson as Aeson
import           Reports    (entries)

main :: IO ()
main = do
  s <- readFile "./TXT190420101137.TAB"
  Aeson.encodeFile "./data.json" $ Aeson.toJSON $ entries s
