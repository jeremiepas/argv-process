-- | What the specs share: a scratch directory and a look at processes.
module Helpers
  ( withDirectory
  , awaitPid
  , awaitGone
  , isAlive
  , sleepAndTell
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (IOException, bracket, try)
import Data.Text (Text)
import System.Directory (createDirectoryIfMissing, doesFileExist, getTemporaryDirectory, removePathForcibly)
import System.FilePath ((</>))
import System.Posix.Process (getProcessID)
import System.Posix.Signals (nullSignal, signalProcess)
import System.Posix.Types (ProcessID)
import Test.Hspec (expectationFailure)
import Text.Read (readMaybe)

-- | A shell script that starts a long @sleep@ in the background, writes its process id into the file
-- named by its first argument, and waits for it.
sleepAndTell :: Text
sleepAndTell = "sleep 60 & echo $! > \"$1\"; wait"

-- | Run an action with an empty scratch directory of that name, removed afterwards.
withDirectory :: String -> (FilePath -> IO a) -> IO a
withDirectory name action = do
  tmp <- getTemporaryDirectory
  pid <- getProcessID
  let dir = tmp </> ("argv-process-test-" <> name <> "-" <> show pid)
  bracket (removePathForcibly dir >> createDirectoryIfMissing True dir >> pure dir) removePathForcibly action

-- | The process id a program wrote into a file, once it is there (at most ten seconds).
awaitPid :: FilePath -> IO ProcessID
awaitPid path = go (0 :: Int)
  where
    go n
      | n > 200 = expectationFailure ("no process id in " <> path) >> pure 0
      | otherwise = do
          exists <- doesFileExist path
          content <- if exists then readFile path else pure ""
          case readMaybe (takeWhile (/= '\n') content) of
            Just pid | '\n' `elem` content -> pure (fromInteger pid)
            _ -> threadDelay 50000 >> go (n + 1)

-- | Whether a process with that id exists.
isAlive :: ProcessID -> IO Bool
isAlive pid = either (const False) (const True) <$> (try (signalProcess nullSignal pid) :: IO (Either IOException ()))

-- | Whether a process is gone within that many microseconds.
awaitGone :: Int -> ProcessID -> IO Bool
awaitGone limit pid = go 0
  where
    go waited = do
      alive <- isAlive pid
      if not alive
        then pure True
        else
          if waited >= limit
            then pure False
            else threadDelay 100000 >> go (waited + 100000)
