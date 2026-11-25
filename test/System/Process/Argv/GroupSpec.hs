-- | Terminating a process group, and the watchdog that does it when the caller dies.
module System.Process.Argv.GroupSpec (spec) where

import Control.Exception (IOException, try)
import System.Environment (getExecutablePath)
import System.FilePath ((</>))
import System.Posix.Signals (sigKILL, signalProcess)
import System.Process (CreateProcess (..), ProcessHandle, StdStream (..), createProcess, getPid, getProcessExitCode, proc, waitForProcess)
import System.Process.Argv.Group
import Test.Hspec

import Data.Text qualified as T
import Helpers (awaitGone, awaitPid, isAlive, sleepAndTell, withDirectory)

-- | Start the script that tells where its sleep is, in a group of its own.
startGroup :: FilePath -> IO ProcessHandle
startGroup pidFile = do
  (_, _, _, process) <-
    createProcess (proc "sh" ["-c", T.unpack sleepAndTell, "sh", pidFile]) {std_in = NoStream, create_group = True}
  pure process

spec :: Spec
spec = do
  describe "terminateGroup" $ do
    it "terminates the process and what it started, and waits for the process" $ withDirectory "group" $ \dir -> do
      let pidFile = dir </> "sleep.pid"
      process <- startGroup pidFile
      sleeper <- awaitPid pidFile
      terminateGroup process
      getProcessExitCode process >>= (`shouldSatisfy` (/= Nothing))
      awaitGone 5000000 sleeper `shouldReturn` True

    it "does nothing for a process that was already waited for" $ do
      (_, _, _, process) <- createProcess (proc "true" []) {create_group = True}
      _ <- waitForProcess process
      terminateGroup process `shouldReturn` ()

  describe "the watchdog" $ do
    it "terminates the group when the caller is killed" $ do
      exe <- getExecutablePath
      withDirectory "watchdog" $ \dir -> do
        let pidFile = dir </> "sleep.pid"
        -- The test suite itself plays the caller: see Spec.hs.
        (_, _, _, caller) <- createProcess (proc exe ["--caller-to-kill", pidFile]) {std_in = NoStream, std_out = NoStream, std_err = NoStream}
        sleeper <- awaitPid pidFile
        Just callerPid <- getPid caller
        signalProcess sigKILL callerPid
        _ <- waitForProcess caller
        awaitGone 8000000 sleeper `shouldReturn` True

    it "gives back the result of the action, and leaves the group alone while the caller lives" $
      withDirectory "watchdog-alive" $ \dir -> do
        let pidFile = dir </> "sleep.pid"
        process <- startGroup pidFile
        sleeper <- awaitPid pidFile
        withWatchdog process (pure (42 :: Int)) `shouldReturn` 42
        isAlive sleeper `shouldReturn` True
        terminateGroup process
        awaitGone 5000000 sleeper `shouldReturn` True

    it "cannot start for a process that was already waited for" $ do
      (_, _, _, process) <- createProcess (proc "true" []) {create_group = True}
      _ <- waitForProcess process
      started <- try (startWatchdog process) :: IO (Either IOException Watchdog)
      either (const True) (const False) started `shouldBe` True
