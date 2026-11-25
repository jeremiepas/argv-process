-- | Run a program from an argument vector, never through a shell, with a time limit and no orphan process.
--
-- The first element of the argument vector is the program, looked up on the @PATH@; each other element
-- reaches it as one argument, whatever characters it holds: spaces, quotes, @;@, @$(...)@ or line ends
-- are never interpreted. Both outputs are captured, read at the same time so that a program that fills
-- one of them never blocks. A text can be given on its standard input.
--
-- The program runs in a process group of its own. When it exceeds its time limit, when the calling
-- thread is interrupted by an exception, or when the calling process dies (even by @SIGKILL@), the whole
-- group is terminated: the program and everything it started. See "System.Process.Argv.Group".
--
-- > import qualified Data.Text.IO as T
-- > import System.Process.Argv
-- >
-- > main :: IO ()
-- > main = do
-- >   result <- runWith defaultOptions {timeLimitSeconds = Just 10} ["git", "log", "-1", "--format=%s"]
-- >   case result of
-- >     Left problem -> T.putStrLn (describeStartError problem)
-- >     Right (Result (Exited 0) out _) -> T.putStr out
-- >     Right (Result end _ err) -> print end >> T.putStr err
--
-- POSIX only: the library depends on @unix@, and the watchdog is an @sh@ script.
module System.Process.Argv
  ( -- * Running a program
    run
  , runWith
    -- * Options
  , Options (..)
  , defaultOptions
    -- * Results
  , Result (..)
  , End (..)
  , StartError (..)
  , describeStartError
  ) where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar, tryPutMVar)
import Control.Exception (IOException, finally, mask, onException, try)
import Control.Monad (void)
import Data.ByteString qualified as BS
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8With, encodeUtf8)
import Data.Text.Encoding.Error (lenientDecode)
import System.IO (Handle, hClose)
import System.Process (CreateProcess (..), ProcessHandle, StdStream (..), createProcess, proc)
import System.Process.Argv.Group (startWatchdog, stopWatchdog, terminateGroup)
import System.Process.Argv.Wait (exitNumber, waitAtMost)
import System.Timeout (timeout)

-- | How to run a program. Start from 'defaultOptions' and change what you need.
data Options = Options
  { stdinText :: Maybe Text
  -- ^ The text written, encoded in UTF-8, on the standard input of the program, which is closed after
  -- it. With 'Nothing' the standard input is closed at once: a program that reads it sees its end.
  , timeLimitSeconds :: Maybe Int
  -- ^ How many seconds the program may run, at least one. Past that it is terminated with its whole
  -- process group, and the run ends with 'TimedOut'. With 'Nothing' there is no limit.
  }
  deriving stock (Eq, Show)

-- | No standard input and no time limit.
defaultOptions :: Options
defaultOptions = Options {stdinText = Nothing, timeLimitSeconds = Nothing}

-- | How a program ended.
data End
  = -- | It ended by itself with this exit code: 0 for success, and the negated signal number when a
    -- signal killed it (-9 for @SIGKILL@).
    Exited Int
  | -- | It exceeded its time limit and was terminated, with its process group.
    TimedOut
  deriving stock (Eq, Show)

-- | What a program did.
data Result = Result
  { resultEnd :: End
  -- ^ How it ended.
  , resultStdout :: Text
  -- ^ Everything it wrote on its standard output before it ended or was terminated, decoded as UTF-8
  -- (an invalid byte becomes U+FFFD).
  , resultStderr :: Text
  -- ^ The same for its standard error.
  }
  deriving stock (Eq, Show)

-- | Why a program could not be started. Nothing ran, or what started was terminated at once.
data StartError
  = -- | The argument vector holds no program.
    EmptyArgumentVector
  | -- | The program could not be started, most often because it does not exist or cannot be executed.
    ProgramNotStarted FilePath IOException
  | -- | The program started, but its watchdog could not, so the program was terminated.
    WatchdogNotStarted IOException
  deriving stock (Eq, Show)

-- | A sentence that says what went wrong, for a log or a user.
describeStartError :: StartError -> Text
describeStartError = \case
  EmptyArgumentVector -> "argv-process: the argument vector is empty"
  ProgramNotStarted program problem -> "argv-process: cannot run " <> T.pack program <> ": " <> T.pack (show problem)
  WatchdogNotStarted problem -> "argv-process: cannot run the program, its watchdog could not start: " <> T.pack (show problem)

-- | Run a program with 'defaultOptions' and wait for it: no standard input, no time limit.
--
-- > run ["printf", "%s", "a; b"]  -- Right (Result (Exited 0) "a; b" "")
run :: [Text] -> IO (Either StartError Result)
run = runWith defaultOptions

-- | Run a program and wait for it, within the time limit of the options.
--
-- A program that exits non-zero is not an error: its code is in 'resultEnd'. 'Left' only when it could
-- not be started.
--
-- Once the program has ended, what is left in its outputs is read for at most five more seconds: a
-- descendant it started in the background may keep them open. Such a descendant is left alone: the group
-- is terminated only on a time limit, an exception, or the death of the caller. After a time limit, the
-- outputs are read for half a second more, and what was written until then is given back.
runWith :: Options -> [Text] -> IO (Either StartError Result)
runWith options argv = case map T.unpack argv of
  [] -> pure (Left EmptyArgumentVector)
  program : arguments -> mask $ \restore -> do
    started <-
      try
        ( createProcess
            (proc program arguments)
              { std_in = CreatePipe
              , std_out = CreatePipe
              , std_err = CreatePipe
              , create_group = True
              }
        )
    case started of
      Left problem -> pure (Left (ProgramNotStarted program problem))
      Right (Just input, Just out, Just err, process) -> do
        watched <- try (startWatchdog process)
        case watched of
          Left problem -> do
            terminateGroup process
            mapM_ closeQuietly [input, out, err]
            pure (Left (WatchdogNotStarted problem))
          Right watchdog ->
            (Right <$> restore (supervise options input out err process) `onException` terminateGroup process)
              `finally` stopWatchdog watchdog
      Right (input, out, err, process) -> do
        terminateGroup process
        mapM_ closeQuietly (catMaybes [input, out, err])
        pure (Left (ProgramNotStarted program (userError "its standard streams could not be attached")))

-- | Feed the program, read both of its outputs, and wait for it within the time limit.
supervise :: Options -> Handle -> Handle -> Handle -> ProcessHandle -> IO Result
supervise options input out err process = do
  output <- newIORef []
  errors <- newIORef []
  outputDone <- newEmptyMVar
  errorsDone <- newEmptyMVar
  outputReader <- forkIO (collect out output outputDone)
  errorsReader <- forkIO (collect err errors errorsDone)
  -- The input is written from a thread of its own: a program that does not read it must not block the wait.
  writer <- forkIO (feed input (stdinText options))
  ended <- waitAtMost (fmap micros (timeLimitSeconds options)) process
  end <- case ended of
    Just code -> do
      drain 5000000 outputDone errorsDone
      pure (Exited (exitNumber code))
    Nothing -> do
      terminateGroup process
      drain 500000 outputDone errorsDone
      pure TimedOut
  -- A descendant that survived may still hold a pipe open: what was read is taken as it is, and the
  -- threads still waiting on a pipe are stopped. The writer closes the input itself.
  mapM_ killThread [outputReader, errorsReader, writer]
  mapM_ closeQuietly [out, err]
  written <- readIORef output
  writtenErrors <- readIORef errors
  pure (Result end (text written) (text writtenErrors))
  where
    micros seconds = max 1 (min seconds (maxBound `div` 1000000)) * 1000000
    text = decodeUtf8With lenientDecode . BS.concat . reverse
    drain limit a b = void (timeout limit (takeMVar a >> takeMVar b))

-- | Write the input, if any, then close the pipe. A program that ends without reading it breaks the pipe:
-- that is not an error.
feed :: Handle -> Maybe Text -> IO ()
feed handle input =
  void (try (maybe (pure ()) (BS.hPut handle . encodeUtf8) input) :: IO (Either IOException ()))
    `finally` closeQuietly handle

-- | Read a pipe to its end, keeping every chunk as it comes, so that what was read is there even if the
-- end never comes.
collect :: Handle -> IORef [BS.ByteString] -> MVar () -> IO ()
collect handle chunks done = go
  where
    go = do
      chunk <- try (BS.hGetSome handle 65536) :: IO (Either IOException BS.ByteString)
      case chunk of
        Right bytes | not (BS.null bytes) -> atomicModifyIORef' chunks (\held -> (bytes : held, ())) >> go
        _ -> closeQuietly handle >> void (tryPutMVar done ())

closeQuietly :: Handle -> IO ()
closeQuietly handle = void (try (hClose handle) :: IO (Either IOException ()))
