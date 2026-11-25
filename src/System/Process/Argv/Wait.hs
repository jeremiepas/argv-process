-- | Waiting for a process without blocking the runtime. Internal to the library.
module System.Process.Argv.Wait
  ( waitAtMost
  , exitNumber
  ) where

import Control.Concurrent (threadDelay)
import System.Exit (ExitCode (..))
import System.Process (ProcessHandle, getProcessExitCode)

-- | Wait for a process for at most that many microseconds, or for as long as it runs with 'Nothing'.
-- 'Nothing' back when it is still running.
--
-- The exit code is asked for again and again, never waited for inside a system call: such a wait would
-- stop every thread of a program built without the threaded runtime, the one that counts the time among them.
waitAtMost :: Maybe Int -> ProcessHandle -> IO (Maybe ExitCode)
waitAtMost limit process = go 0
  where
    pause = 20000
    go waited = do
      code <- getProcessExitCode process
      case code of
        Just _ -> pure code
        Nothing
          | maybe False (waited >=) limit -> pure Nothing
          | otherwise -> threadDelay pause >> go (waited + pause)

-- | An exit code as a number: 0 for success, and a negative signal number for a program killed by a signal.
exitNumber :: ExitCode -> Int
exitNumber = \case
  ExitSuccess -> 0
  ExitFailure n -> n
