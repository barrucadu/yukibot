{-# LANGUAGE GADTs #-}

-- |
-- Module      : Yukibot.Backend
-- Copyright   : (c) 2016 Michael Walker
-- License     : MIT
-- Stability   : experimental
-- Portability : GADTs
module Yukibot.Backend
  ( Event(..)
  , Message(..)

    -- * Starting and stopping
  , BackendHandle
  , startBackend
  , stopBackend
  , awaitStart
  , awaitStop
  -- ** STM
  , stopBackendSTM
  , awaitStartSTM
  , awaitStopSTM
  , hasStartedSTM
  , hasStoppedSTM

  -- * Interaction
  , BackendTerminatedException(..)
  , BackendAction(..)
  , sendAction
  -- ** STM
  , sendActionSTM

  -- * Miscellaneous
  , describeBackend

  -- * For library authors
  , Backend(..)
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Monad (void, when)
import Control.Monad.Catch (Exception, throwM)
import Data.Text (Text)

-- | A representation of a backend, it is parameterised by the channel
-- and user types.
--
-- TODO: Pass configuration in.
--
-- TODO: Have configuration determine description.
data Backend channel user where
  Backend :: { initialise :: ((BackendHandle channel user -> Event channel user) -> IO ()) -> IO a
             , run :: TQueue (BackendAction channel user) -> a -> IO ()
             , describe :: Text
             } -> Backend channel user

-------------------------------------------------------------------------------
-- Starting and stopping

-- | An abstract handle to a backend, which can be used to interact
-- with it.
data BackendHandle channel user = BackendHandle
  { msgQueue    :: TQueue (BackendAction channel user)
  , hasStarted  :: TVar Bool
  , hasStopped  :: TVar Bool
  , description :: Text
  }
  deriving Eq

-- | Start executing a backend in another thread.
--
-- This will return immediately. If you want to block until the
-- backend is ready, see 'awaitStart'.
startBackend :: (Event channel user -> IO ())
  -- ^ Process received events
  -> Backend channel user
  -> IO (BackendHandle channel user)
startBackend onReceive b@(Backend setup exec _) = do
  h <- createHandle b
  forkAndRunBackend h (setup $ \ef -> onReceive (ef h)) (exec $ msgQueue h)
  pure h

-- | Tell a backend to terminate. If the backend has already
-- terminated, throws 'BackendTerminatedException'.
--
-- This will return immediately, and it is possible that messages from
-- the backend will be received between calling this function and it
-- terminating. If you want to block until the backend is closed, see
-- 'awaitStop'.
stopBackend :: BackendHandle channel user -> IO ()
stopBackend = atomically . stopBackendSTM

-- | STM variant of 'stopBackend'.
stopBackendSTM :: BackendHandle channel user -> STM ()
stopBackendSTM b = sendActionSTM b Terminate

-- | Block until the backend is initialised. If the backend has
-- terminated, or terminates while waiting, this unblocks and throws
-- 'BackendTerminatedException'.
awaitStart :: BackendHandle channel user -> IO ()
awaitStart = atomically . awaitStartSTM

-- | STM variant of 'awaitStart'.
awaitStartSTM :: BackendHandle channel user -> STM ()
awaitStartSTM b = do
  throwOnStop b
  check =<< hasStartedSTM b

-- | Block until the backend terminates. This never throws
-- 'BackendTerminatedException'.
awaitStop :: BackendHandle channel user -> IO ()
awaitStop = atomically . awaitStopSTM

-- | STM variant of 'awaitStop'.
awaitStopSTM :: BackendHandle channel user -> STM ()
awaitStopSTM b = check =<< hasStoppedSTM b

-- | Check if the backend has finished its initialisation and entered
-- the main loop.
hasStartedSTM :: BackendHandle channel user -> STM Bool
hasStartedSTM = readTVar . hasStarted

-- | Check if the backend has terminated.
hasStoppedSTM :: BackendHandle channel user -> STM Bool
hasStoppedSTM = readTVar . hasStopped

-------------------------------------------------------------------------------
-- Interaction

-- | TODO: A richer message type, in its own module.
newtype Message = Message Text
  deriving (Eq, Read, Show)

-- | TODO: A richer event type, in its own module.
data Event channel user = Event (BackendHandle channel user) (Maybe channel) user Text

-- | All the actions a backend can perform.
data BackendAction channel user
  = Join channel
  -- ^ Join a new channel.
  | Leave channel
  -- ^ Leave a current channel.
  | Say channel [user] Message
  -- ^ Send a message to a channel, optionally addressed to a collection of users.
  | Whisper user Message
  -- ^ Send a message to a user.
  | Terminate
  -- ^ Gracefully disconnect and stop running.
  deriving (Eq, Read, Show)

data BackendTerminatedException = BackendTerminatedException
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

instance Exception BackendTerminatedException

-- | Instruct the backend to perform an action. If the backend has
-- terminated, throws 'BackendTerminatedException'.
--
-- Note: @sendAction b Terminate@ is exactly the same as @stopBackend b@
sendAction :: BackendHandle channel user -> BackendAction channel user -> IO ()
sendAction b = atomically . sendActionSTM b

-- | STM variant of 'sendAction'.
sendActionSTM :: BackendHandle channel user -> BackendAction channel user -> STM ()
sendActionSTM b a = do
  throwOnStop b
  writeTQueue (msgQueue b) a

-------------------------------------------------------------------------------
-- Miscellaneous

-- | Return a textual description of a backend
describeBackend :: BackendHandle channel user -> Text
describeBackend = description

-------------------------------------------------------------------------------
-- Internal

-- | Create a new 'BackendHandle'.
createHandle :: Backend channel user -> IO (BackendHandle channel user)
createHandle b = atomically $ do
  queue    <- newTQueue
  startvar <- newTVar False
  stopvar  <- newTVar False

  pure BackendHandle { msgQueue = queue
                     , hasStarted = startvar
                     , hasStopped = stopvar
                     , description = describe b
                     }

-- | Initialise and run a backend in a new thread.
forkAndRunBackend :: BackendHandle channel user -> IO a -> (a -> IO ()) -> IO ()
forkAndRunBackend h setup exec = void . forkIO $ do
  a <- setup
  atomically $ writeTVar (hasStarted h) True
  exec a
  atomically $ writeTVar (hasStopped h) True

-- | Throw a 'BackendTerminatedException' if it has stopped.
throwOnStop :: BackendHandle channel user -> STM ()
throwOnStop b = do
  stopped <- hasStoppedSTM b
  when stopped (throwM BackendTerminatedException)
