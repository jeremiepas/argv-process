module Main (main) where

import Data.Text qualified as T
import Helpers (sleepAndTell)
import System.Environment (getArgs)
import System.Process.Argv (runWith, defaultOptions, timeLimitSeconds)
import System.Process.Argv.GroupSpec qualified
import System.Process.ArgvSpec qualified
import Test.Hspec (describe, hspec)

main :: IO ()
main = do
  arguments <- getArgs
  case arguments of
    -- The test suite runs itself with these arguments to play a caller that gets killed: it starts a
    -- shell that starts a long sleep, writes the process id of the sleep into the file, and waits.
    ["--caller-to-kill", pidFile] ->
      () <$ runWith defaultOptions {timeLimitSeconds = Just 60} ["sh", "-c", sleepAndTell, "sh", T.pack pidFile]
    _ -> hspec $ do
      describe "System.Process.Argv" System.Process.ArgvSpec.spec
      describe "System.Process.Argv.Group" System.Process.Argv.GroupSpec.spec
