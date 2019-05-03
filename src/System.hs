module System
  ( nthArg
  , argOr
  , putStderr
  ) where

import qualified Control.Applicative as Ap
import qualified Data.Maybe          as M
import qualified System.Environment  as Env
import qualified System.IO           as Sys
import           Utils               (nth)

nthArg :: Int -> IO (Maybe String)
nthArg = Ap.liftA2 nth Env.getArgs . pure

argOr :: Int -> String -> IO String
argOr n def = M.fromMaybe def <$> nthArg n

putStderr :: String -> IO ()
putStderr = Sys.hPutStrLn Sys.stderr
