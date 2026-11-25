-- | Running real programs: an argument vector, never a shell.
module System.Process.ArgvSpec (spec) where

import Control.Concurrent (threadDelay)
import Data.Text (Text)
import Data.Text qualified as T
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.Process.Argv
import System.Timeout (timeout)
import Test.Hspec

import Helpers (awaitGone, awaitPid, sleepAndTell, withDirectory)

-- | Run a program that must start, and give back what it did.
started :: Options -> [Text] -> IO Result
started options argv =
  runWith options argv >>= \case
    Right result -> pure result
    Left problem -> expectationFailure (T.unpack (describeStartError problem)) >> error "unreachable"

limited :: Int -> Options
limited seconds = defaultOptions {timeLimitSeconds = Just seconds}

spec :: Spec
spec = do
  describe "the argument vector" $ do
    it "gives a value that holds shell metacharacters to the program as one literal argument" $
      -- The program prints how many arguments it received, then the first.
      run ["sh", "-c", "printf '%s\\n' \"$#\" \"$1\"", "sh", "x; rm -rf y"]
        `shouldReturn` Right (Result (Exited 0) "1\nx; rm -rf y\n" "")

    it "runs no second command, whatever an argument holds" $ withDirectory "argv" $ \dir -> do
      let marker = T.pack (dir </> "second-command-ran")
          hostile = T.concat ["x; touch ", marker, " ; $(touch ", marker, ") `touch ", marker, "` | touch ", marker, " && touch ", marker]
      run ["printf", "%s", hostile] `shouldReturn` Right (Result (Exited 0) hostile "")
      doesFileExist (T.unpack marker) `shouldReturn` False

    it "keeps one element one argument, with spaces, quotes, line ends and globs in it" $ do
      let value = "two words 'quoted' \"double\"\nsecond line\t*"
      run ["sh", "-c", "printf '%s|' \"$#\" \"$1\" \"$2\"", "sh", "<" <> value <> ">", value <> value]
        `shouldReturn` Right (Result (Exited 0) ("2|<" <> value <> ">|" <> value <> value <> "|") "")

    it "passes an empty argument as an argument" $
      run ["sh", "-c", "printf '%s' \"$#\"", "sh", ""] `shouldReturn` Right (Result (Exited 0) "1" "")

  describe "starting" $ do
    it "is refused, and nothing runs, when the argument vector is empty" $
      run [] `shouldReturn` Left EmptyArgumentVector

    it "fails, and does not throw, when the program does not exist" $ do
      result <- run ["no-such-program-of-the-argv-process-tests", "x"]
      case result of
        Left problem@(ProgramNotStarted program _) -> do
          program `shouldBe` "no-such-program-of-the-argv-process-tests"
          T.unpack (describeStartError problem) `shouldContain` "argv-process: cannot run no-such-program-of-the-argv-process-tests"
        other -> expectationFailure (show other)

  describe "the result" $ do
    it "is the exit code and both outputs" $
      run ["sh", "-c", "echo partial; echo why >&2; exit 7"]
        `shouldReturn` Right (Result (Exited 7) "partial\n" "why\n")

    it "gives the negated signal number of a program killed by a signal" $
      run ["sh", "-c", "kill -9 $$"] `shouldReturn` Right (Result (Exited (-9)) "" "")

    it "decodes the outputs as UTF-8, an invalid byte becoming U+FFFD" $
      run ["printf", "caf\\303\\251 \\377"] `shouldReturn` Right (Result (Exited 0) "café \xFFFD" "")

    it "reads both outputs without blocking on either" $ do
      let script = "head -c 300000 /dev/zero | tr '\\0' e >&2; head -c 300000 /dev/zero | tr '\\0' o"
      result <- timeout 20000000 (started defaultOptions ["sh", "-c", script])
      fmap (\r -> (resultEnd r, T.length (resultStdout r), T.length (resultStderr r))) result
        `shouldBe` Just (Exited 0, 300000, 300000)

    it "comes back when a descendant keeps the outputs open after the program ended" $ do
      result <- timeout 9000000 (started defaultOptions ["sh", "-c", "echo hi; sleep 8 &"])
      result `shouldBe` Just (Result (Exited 0) "hi\n" "")

  describe "the standard input" $ do
    it "is closed at once when there is no input" $
      timeout 10000000 (run ["cat"]) `shouldReturn` Just (Right (Result (Exited 0) "" ""))

    it "receives the input text, encoded in UTF-8" $ do
      let input = "first line\nsecond, with é and ✓\n"
      run' <- started defaultOptions {stdinText = Just input} ["cat"]
      run' `shouldBe` Result (Exited 0) input ""

    it "does not block the run when the program does not read it" $ do
      let input = T.replicate 1000000 "x"
      timeout 10000000 (started defaultOptions {stdinText = Just input} ["true"])
        `shouldReturn` Just (Result (Exited 0) "" "")

  describe "the time limit" $ do
    it "does not touch a program that ends in time" $
      started (limited 5) ["printf", "ok"] `shouldReturn` Result (Exited 0) "ok" ""

    it "terminates a program that exceeds it with everything it started, and keeps what it wrote" $
      withDirectory "limit" $ \dir -> do
        let marker = dir </> "child-ran"
        result <- timeout 10000000 (started (limited 1) ["sh", "-c", "printf started; sleep 3; touch \"$1\"", "sh", T.pack marker])
        result `shouldBe` Just (Result TimedOut "started" "")
        -- The child that would have touched the marker was in the process group, and was killed with it.
        threadDelay 3500000
        doesFileExist marker `shouldReturn` False

    it "kills a program that ignores SIGTERM" $ do
      result <- timeout 8000000 (started (limited 1) ["sh", "-c", "trap '' TERM; sleep 30"])
      fmap resultEnd result `shouldBe` Just TimedOut

  describe "an exception in the caller" $
    it "terminates the program and everything it started" $ withDirectory "exception" $ \dir -> do
      let pidFile = dir </> "sleep.pid"
      interrupted <- timeout 1500000 (run ["sh", "-c", sleepAndTell, "sh", T.pack pidFile])
      interrupted `shouldBe` Nothing
      sleeper <- awaitPid pidFile
      awaitGone 5000000 sleeper `shouldReturn` True
