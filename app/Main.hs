module Main
  ( main
  ) where

import qualified Data.Aeson         as Aeson
import           Parsing            (parseEntries)
import qualified System.Environment as Env
import qualified System.IO          as Sys

putStderr :: String -> IO ()
putStderr = Sys.hPutStrLn Sys.stderr

doStuff :: FilePath -> FilePath -> IO ()
doStuff input output =
  Aeson.encodeFile output =<< Aeson.toJSON . parseEntries <$> readFile input

main :: IO ()
main = do
  (input:output:_) <- Env.getArgs -- TODO handle missing files
  putStderr $ "Processing statements from: \"" ++ input ++ "\""
  doStuff input output
  putStderr $ "Data written to: \"" ++ input ++ "\""
