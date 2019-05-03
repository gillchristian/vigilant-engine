module Main
  ( main
  ) where

import qualified Data.Aeson as Aeson
import           Parsing    (parseEntries)
import           System     (argOr, putStderr)

parse :: String -> Aeson.Value
parse = Aeson.toJSON . parseEntries

parseToFile :: FilePath -> FilePath -> IO ()
parseToFile input output =
  pure parse <*> readFile input >>= Aeson.encodeFile output

main :: IO ()
main = do
  input <- argOr 0 "statements.txt"
  output <- argOr 1 "data.json"
  putStderr $ "Processing statements from: \"" ++ input ++ "\""
  parseToFile input output
  putStderr $ "Data written to: \"" ++ output ++ "\""
