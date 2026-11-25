-- | The process group of a program, and the watchdog that terminates it when the caller dies.
--
-- 'System.Process.Argv.runWith' uses these for every program it runs. They are exported for a caller
-- that starts a process itself, with 'System.Process.createProcess', and wants the same guarantees. The
-- process must have been started with @'System.Process.create_group' = True@: its process id is then the
-- id of a group of its own, which holds it and everything it starts (unless a descendant leaves the
-- group, with @setsid@ for instance).
--
-- > (_, _, _, process) <- createProcess (proc "make" ["-j8"]) {create_group = True}
-- > withWatchdog process (waitForProcess process)
--
-- POSIX only. The watchdog is a small @sh@ script, so @sh@ must be on the @PATH@.
module System.Process.Argv.Group
  ( -- * Terminating a group
    terminateGroup
    -- * Watchdog
  , Watchdog
  , startWatchdog
  , stopWatchdog
  , withWatchdog
  ) where

import Control.Exception (IOException, bracket, try)
import Control.Monad (void)
import System.Posix.Process (getProcessID)
import System.Posix.Signals (Signal, sigKILL, sigTERM, signalProcess, signalProcessGroup)
import System.Posix.Types (ProcessGroupID)
import System.Process (CreateProcess (..), ProcessHandle, StdStream (..), createProcess, getPid, proc)
import System.Process.Argv.Wait (waitAtMost)

-- | Terminate a process and the whole of its process group: the group is sent @SIGTERM@, the process is
-- given two seconds to end, then whatever is left of the group is sent @SIGKILL@. Returns once the
-- process has ended and has been waited for.
--
-- Does nothing when the process has already been waited for: its id is no longer known, and may belong
-- to another program by now.
terminateGroup :: ProcessHandle -> IO ()
terminateGroup process = do
  pid <- getPid process
  case pid of
    Nothing -> pure ()
    Just group -> do
      signal sigTERM group
      _ <- waitAtMost (Just 2000000) process
      -- Whatever of the group is still there, the process itself or what it started, is killed.
      signal sigKILL group
      void (waitAtMost Nothing process)
  where
    signal :: Signal -> ProcessGroupID -> IO ()
    signal sig group = void (try (signalProcessGroup sig group) :: IO (Either IOException ()))

-- | A running watchdog, from 'startWatchdog'.
newtype Watchdog = Watchdog ProcessHandle

-- | Start a watchdog for the process group of a process. The watchdog is a separate process that checks
-- every second that the caller (the current process) is still alive. When the caller dies, even by
-- @SIGKILL@, it terminates the group: @SIGTERM@, then @SIGKILL@ two seconds later. It runs in a process
-- group of its own, so that a signal sent to the group of the caller does not end it before it has done
-- its work.
--
-- Stop it with 'stopWatchdog' once the process has ended, or use 'withWatchdog'. Throws an
-- 'IOException' when the watchdog cannot be started, or when the process has already been waited for.
startWatchdog :: ProcessHandle -> IO Watchdog
startWatchdog process = do
  child <- getPid process
  caller <- getProcessID
  case child of
    Nothing -> ioError (userError "argv-process: the process has already been waited for, there is no group to watch")
    Just group -> do
      let script =
            unlines
              [ "caller=$1; group=$2"
              , "while kill -0 \"$caller\" 2>/dev/null; do sleep 1; done"
              , "kill -TERM -\"$group\" 2>/dev/null || true"
              , "sleep 2"
              , "kill -KILL -\"$group\" 2>/dev/null || true"
              ]
      (_, _, _, watchdog) <-
        createProcess
          (proc "sh" ["-c", script, "argv-process-watchdog", show caller, show group])
            { std_in = NoStream
            , std_out = NoStream
            , std_err = NoStream
            , create_group = True
            }
      pure (Watchdog watchdog)

-- | Stop a watchdog, leaving the group it watched alone. Killing the watchdog is enough: its only child
-- is a @sleep 1@ that ends on its own.
stopWatchdog :: Watchdog -> IO ()
stopWatchdog (Watchdog watchdog) = do
  pid <- getPid watchdog
  case pid of
    Nothing -> pure ()
    Just p -> void (try (signalProcess sigKILL p) :: IO (Either IOException ()))
  void (waitAtMost Nothing watchdog)

-- | Run an action with a watchdog on the group of a process: 'startWatchdog' before, 'stopWatchdog'
-- after, even when the action throws.
withWatchdog :: ProcessHandle -> IO a -> IO a
withWatchdog process action = bracket (startWatchdog process) stopWatchdog (const action)
