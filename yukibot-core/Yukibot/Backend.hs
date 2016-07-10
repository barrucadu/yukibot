-- |
-- Module      : Yukibot.Backend
-- Copyright   : (c) 2016 Michael Walker
-- License     : MIT
-- Stability   : experimental
-- Portability : portable
module Yukibot.Backend
  ( -- * Starting and stopping
    Backend
  , BackendHandle
  , startBackend
  , startInstantiatedBackend
  , stopBackend
  , awaitStart
  , awaitStop
  -- ** STM
  , awaitStartSTM
  , awaitStopSTM
  , hasStartedSTM
  , hasStoppedSTM

  -- * Interaction
  , BackendTerminatedException(..)
  , Action(..)
  , sendAction

  -- * Miscellaneous
  , describeBackend
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Monad (void, when)
import Control.Monad.Catch (throwM)
import Data.Text (Text)

import Yukibot.Log (loggerFromBackend, rawLoggerFromBackend)
import Yukibot.Types

-------------------------------------------------------------------------------
-- Starting and stopping

-- | Start executing a backend, with logging, in another thread.
--
-- This will return immediately. If you want to block until the
-- backend is ready, see 'awaitStart'.
startBackend :: (Event -> IO ())
  -- ^ Process received events
  -> BackendName
  -- ^ The name of the backend (e.g. \"irc\")
  -> Text
  -- ^ The name of the instance (e.g. \"irc.freenode.net\")
  -> Int
  -- ^ The index of this particular instance in the table array (0 if
  -- there is no array).
  -> Backend -> IO BackendHandle
startBackend onReceive bname sname index b@(Backend setup exec _ _ _) = do
  (h, eventLogger) <- createHandle bname sname index b

  let rawlogger = rawLoggerFromBackend b
  let receive ef = let e = ef h in eventLogger e >> onReceive e

  forkAndRunBackend h (setup rawlogger receive) (exec $ msgQueue h)
  pure h

-- | Start an instantiated backend.
startInstantiatedBackend :: (Event -> IO ()) -> InstantiatedBackend -> IO BackendHandle
startInstantiatedBackend onReceive ib = startBackend onReceive (instBackendName ib) (instSpecificName ib) (instIndex ib) (instBackend ib)

-- | Tell a backend to terminate. If the backend has already
-- terminated, throws 'BackendTerminatedException'.
--
-- This will return immediately, and it is possible that messages from
-- the backend will be received between calling this function and it
-- terminating. If you want to block until the backend is closed, see
-- 'awaitStop'.
stopBackend :: BackendHandle -> IO ()
stopBackend h = sendAction h Terminate

-- | Block until the backend is initialised. If the backend has
-- terminated, or terminates while waiting, this unblocks and throws
-- 'BackendTerminatedException'.
awaitStart :: BackendHandle -> IO ()
awaitStart = atomically . awaitStartSTM

-- | STM variant of 'awaitStart'.
awaitStartSTM :: BackendHandle -> STM ()
awaitStartSTM b = do
  throwOnStop b
  check =<< hasStartedSTM b

-- | Block until the backend terminates. This never throws
-- 'BackendTerminatedException'.
awaitStop :: BackendHandle -> IO ()
awaitStop = atomically . awaitStopSTM

-- | STM variant of 'awaitStop'.
awaitStopSTM :: BackendHandle -> STM ()
awaitStopSTM b = check =<< hasStoppedSTM b

-- | Check if the backend has finished its initialisation and entered
-- the main loop.
hasStartedSTM :: BackendHandle -> STM Bool
hasStartedSTM = readTVar . hasStarted

-- | Check if the backend has terminated.
hasStoppedSTM :: BackendHandle -> STM Bool
hasStoppedSTM = readTVar . hasStopped

-------------------------------------------------------------------------------
-- Interaction

-- | Instruct the backend to perform an action. If the backend has
-- terminated, throws 'BackendTerminatedException'.
--
-- Note: @sendAction h Terminate@ is exactly the same as @stopBackend h@
sendAction :: BackendHandle -> Action -> IO ()
sendAction h a = do
  atomically $ writeTQueue (msgQueue h) a
  actionLogger h a

-------------------------------------------------------------------------------
-- Miscellaneous

-- | Return a textual description of a backend
describeBackend :: BackendHandle -> Text
describeBackend = description

-------------------------------------------------------------------------------
-- Internal

-- | Create a new 'BackendHandle' and event logger.
createHandle :: BackendName -> Text -> Int -> Backend -> IO (BackendHandle, Event -> IO ())
createHandle bname sname index b = atomically $ do
  queue    <- newTQueue
  startvar <- newTVar False
  stopvar  <- newTVar False

  let logger = loggerFromBackend b
  let handle = BackendHandle { msgQueue = queue
                             , hasStarted = startvar
                             , hasStopped = stopvar
                             , description = describe b
                             , actionLogger = loggerAction logger
                             , backendName = bname
                             , specificName = sname
                             , backendIndex = index
                             }

  pure (handle, loggerEvent logger)

-- | Initialise and run a backend in a new thread.
forkAndRunBackend :: BackendHandle
  -> IO a
  -> (a -> IO ())
  -> IO ()
forkAndRunBackend h setup exec = void . forkIO $ do
  a <- setup
  atomically $ writeTVar (hasStarted h) True
  exec a
  atomically $ writeTVar (hasStopped h) True

-- | Throw a 'BackendTerminatedException' if it has stopped.
throwOnStop :: BackendHandle -> STM ()
throwOnStop b = do
  stopped <- hasStoppedSTM b
  when stopped (throwM BackendTerminatedException)
