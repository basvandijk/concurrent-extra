{-# LANGUAGE DeriveDataTypeable
           , NoImplicitPrelude
           , UnicodeSyntax
  #-}

-------------------------------------------------------------------------------
-- |
-- Module     : Control.Concurrent.Thread
-- Copyright  : (c) 2010 Bas van Dijk & Roel van Dijk
-- License    : BSD3 (see the file LICENSE)
-- Maintainer : Bas van Dijk <v.dijk.bas@gmail.com>
--            , Roel van Dijk <vandijk.roel@gmail.com>
--
-- Standard threads extended with the ability to wait for their termination.
--
-- Inspired by: <http://hackage.haskell.org/package/threadmanager>
--
-- This module re-implements several functions from @Control.Concurrent@. Avoid
-- ambiguities by importing one or both qualified. We suggest importing this
-- module like:
--
-- @
-- import qualified Control.Concurrent.Thread as Thread ( ... )
-- @
--
-------------------------------------------------------------------------------

module Control.Concurrent.Thread
  ( ThreadId
  , threadId
  , forkIO
  , forkOS
  , wait
  , waitTimeout
  , isRunning
  , killThread
  , killThreadTimeout
  , throwTo
  ) where


-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

-- from base:
import Control.Applicative ( (<$>) )
import Control.Exception   ( Exception, SomeException
                           , AsyncException(ThreadKilled)
                           , catch, block, unblock
                           )
import Control.Monad       ( return, (>>=), fail, (>>), fmap )
import Data.Bool           ( Bool )
import Data.Eq             ( Eq, (==) )
import Data.Function       ( ($), on )
import Data.Int            ( Int )
import Data.Maybe          ( Maybe(Nothing, Just), isNothing, isJust )
import Data.Ord            ( Ord, compare )
import Data.Typeable       ( Typeable )
import System.IO           ( IO )

import Text.Show           ( Show, show )

import qualified Control.Concurrent as Conc
    ( ThreadId, forkIO, forkOS, throwTo )

-- from base-unicode-symbols:
import Data.Function.Unicode ( (∘) )

-- from concurrent-extra:
import           Control.Concurrent.Broadcast ( Broadcast )
import qualified Control.Concurrent.Broadcast as Broadcast
    ( new, write, read, tryRead, readTimeout )

import Utils ( void )


-------------------------------------------------------------------------------
-- Threads
-------------------------------------------------------------------------------

{-|
A 'ThreadId' is an abstract type representing a handle to a thread. 'ThreadId'
is an instance of 'Eq', 'Ord' and 'Show', where the 'Ord' instance implements an
arbitrary total ordering over 'ThreadId's. The 'Show' instance lets you convert
an arbitrary-valued 'ThreadId' to string form; showing a 'ThreadId' value is
occasionally useful when debugging or diagnosing the behaviour of a concurrent
program.

Note: This is a wrapper around 'Conc.ThreadId'.
-}
data ThreadId = ThreadId
    { stopped   ∷ Broadcast (Maybe SomeException)
      -- | Extract the underlying 'Conc.ThreadId'.
    , threadId  ∷ Conc.ThreadId
    } deriving Typeable

instance Eq ThreadId where
    (==) = (==) `on` threadId

instance Ord ThreadId where
    compare = compare `on` threadId

instance Show ThreadId where
    show = show ∘ threadId

{-|
Internally used function which generalises 'forkIO' and 'forkOS'. Parametrised
by the function which does the actual forking.
-}
fork ∷ (IO () → IO Conc.ThreadId) → IO () → IO ThreadId
fork doFork a = do
  stop ← Broadcast.new
  tid ← block $ doFork
              $ let writeStop = Broadcast.write stop
                in catch (unblock a >> writeStop Nothing)
                         (writeStop ∘ Just)
  return $ ThreadId stop tid

{-|
Sparks off a new thread to run the given 'IO' computation and returns the
'ThreadId' of the newly created thread.

The new thread will be a lightweight thread; if you want to use a foreign
library that uses thread-local storage, use 'forkOS' instead.

GHC note: the new thread inherits the blocked state of the parent (see
'Control.Exception.block').

The newly created thread has an exception handler that discards the exceptions
@BlockedOnDeadMVar@, @BlockedIndefinitely@, and @ThreadKilled@, and passes all
other exceptions to the uncaught exception handler (see
@setUncaughtExceptionHandler@).
-}
forkIO ∷ IO () → IO ThreadId
forkIO = fork Conc.forkIO

{-|
Like 'forkIO', this sparks off a new thread to run the given 'IO' computation
and returns the 'ThreadId' of the newly created thread.

Unlike 'forkIO', 'forkOS' creates a /bound/ thread, which is necessary if you
need to call foreign (non-Haskell) libraries that make use of thread-local
state, such as OpenGL (see 'Control.Concurrent').

Using 'forkOS' instead of 'forkIO' makes no difference at all to the scheduling
behaviour of the Haskell runtime system. It is a common misconception that you
need to use 'forkOS' instead of 'forkIO' to avoid blocking all the Haskell
threads when making a foreign call; this isn't the case. To allow foreign calls
to be made without blocking all the Haskell threads (with GHC), it is only
necessary to use the @-threaded@ option when linking your program, and to make
sure the foreign import is not marked @unsafe@.
-}
forkOS ∷ IO () → IO ThreadId
forkOS = fork Conc.forkOS

{-|
Block until the given thread is terminated.

Returns 'Nothing' if the thread terminated normally or 'Just' @e@ if the
exception @e@ was thrown in the thread and wasn't caught.
-}
wait ∷ ThreadId → IO (Maybe SomeException)
wait = Broadcast.read ∘ stopped

{-|
Block until the given thread is terminated or until a timer expires.

Returns 'Nothing' if a timeout occurred or 'Just' the result 'wait' would return
when the thread finished within the specified time.

The timeout is specified in microseconds.
-}
waitTimeout ∷ ThreadId → Int → IO (Maybe (Maybe SomeException))
waitTimeout = Broadcast.readTimeout ∘ stopped

{-|
Returns 'True' if the given thread is currently running.

Notice that this observation is only a snapshot of thread's state. By the time a
program reacts on its result it may already be out of date.
-}
isRunning ∷ ThreadId → IO Bool
isRunning = fmap isNothing ∘ Broadcast.tryRead ∘ stopped


-------------------------------------------------------------------------------
-- Convenience functions
-------------------------------------------------------------------------------

{-|
'killThread' terminates the given thread (GHC only). Any work already done by
the thread isn't lost: the computation is suspended until required by another
thread. The memory used by the thread will be garbage collected if it isn't
referenced from anywhere. The 'killThread' function is defined in terms of
'throwTo'.

This function blocks until the target thread is terminated. It is a no-op if the
target thread has already completed.
-}
killThread ∷ ThreadId → IO ()
killThread t = throwTo t ThreadKilled >> void (wait t)

{-|
Like 'killThread' but with a timeout. Returns 'True' if the target thread was
terminated without the given amount of time, 'False' otherwise.

Note that even when a timeout occurs, the target thread can still terminate at a
later time as a direct result of calling this function.
-}
killThreadTimeout ∷ ThreadId → Int → IO Bool
killThreadTimeout t time = do throwTo t ThreadKilled
                              isJust <$> waitTimeout t time

{-|
'throwTo' raises an arbitrary exception in the target thread (GHC only).

'throwTo' does not return until the exception has been raised in the target
thread. The calling thread can thus be certain that the target thread has
received the exception. This is a useful property to know when dealing with race
conditions: eg. if there are two threads that can kill each other, it is
guaranteed that only one of the threads will get to kill the other.

If the target thread is currently making a foreign call, then the exception will
not be raised (and hence 'throwTo' will not return) until the call has
completed. This is the case regardless of whether the call is inside a 'block'
or not.

Important note: the behaviour of 'throwTo' differs from that described in the
paper \"Asynchronous exceptions in Haskell\"
(<http://research.microsoft.com/~simonpj/Papers/asynch-exns.htm>). In the paper,
'throwTo' is non-blocking; but the library implementation adopts a more
synchronous design in which 'throwTo' does not return until the exception is
received by the target thread. The trade-off is discussed in Section 9 of the
paper. Like any blocking operation, 'throwTo' is therefore interruptible (see
Section 5.3 of the paper).

There is currently no guarantee that the exception delivered by 'throwTo' will
be delivered at the first possible opportunity. In particular, a thread may
'unblock' and then re-'block' exceptions without receiving a pending
'throwTo'. This is arguably undesirable behaviour.
-}
throwTo ∷ Exception e ⇒ ThreadId → e → IO ()
throwTo = Conc.throwTo ∘ threadId


-- The End ---------------------------------------------------------------------
