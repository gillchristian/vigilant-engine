module Main
  ( main
  ) where

import qualified Data.Aeson         as Aeson
import           Parsing            (entries)
import qualified System.Environment as Env
import qualified System.IO          as Sys

processInput :: String -> IO Aeson.Value
processInput file = Aeson.toJSON . entries <$> readFile file

main :: IO ()
main = do
  (sts:out:_) <- Env.getArgs -- TODO handle missing files
  processInput sts >>= Aeson.encodeFile out
  Sys.hPutStrLn Sys.stderr $ "Processed statements from: \"" ++ sts ++ "\""
  Sys.hPutStrLn Sys.stderr $ "Data written to: \"" ++ out ++ "\""
