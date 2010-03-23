{-# LANGUAGE CPP, DeriveDataTypeable, NoImplicitPrelude, UnicodeSyntax #-}

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

    -- * Forking threads
  , forkIO
  , forkOS

    -- * Waiting on threads
  , wait
  , wait_
  , unsafeWait
  , unsafeWait_

    -- ** Waiting with a timeout
  , waitTimeout
  , waitTimeout_
  , unsafeWaitTimeout
  , unsafeWaitTimeout_

    -- * Quering thread status
  , isRunning

    -- * Convenience functions
  , throwTo
  , killThread
  , killThreadTimeout

    -- * Thread groups
  , Group
  , newGroup
  , forkIOInGroup
  , forkOSInGroup
  , waitAll
  ) where


-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

-- from base:
import Control.Exception  ( Exception, SomeException
                          , AsyncException(ThreadKilled)
                          , try, blocked, block, unblock
                          )
import Control.Concurrent ( myThreadId )
import qualified Control.Concurrent as Conc
                               ( ThreadId, forkIO, forkOS, throwTo )
import Control.Concurrent.MVar ( MVar, newMVar, takeMVar, putMVar, readMVar )
#ifdef __HADDOCK__
import Control.Exception  ( BlockedIndefinitelyOnMVar, BlockedIndefinitelyOnSTM )
#endif
import Control.Monad      ( return, (>>=), (>>), fail, mapM_ )
import Data.Bool          ( Bool(..) )
import Data.Eq            ( Eq, (==) )
import Data.Either        ( Either(..), either )
import Data.Function      ( ($), on, const )
import Data.Functor       ( fmap, (<$>) )
import Data.Maybe         ( Maybe(..), maybe, isNothing, isJust )
import Data.Ord           ( Ord, compare )
import Data.Typeable      ( Typeable )
import Prelude            ( Integer, ($!) )
import System.IO          ( IO )
import Text.Show          ( Show, show )

-- from base-unicode-symbols:
import Data.Function.Unicode ( (∘) )
import Data.Eq.Unicode       ( (≡) )

-- from concurrent-extra:
import qualified Control.Concurrent.Broadcast as Broadcast ( new )
import Control.Concurrent.Broadcast
    ( Broadcast, broadcast, listen, tryListen, listenTimeout )

import Utils ( void, ifM, throwInner, deleteWhen' )


-------------------------------------------------------------------------------
-- Threads
-------------------------------------------------------------------------------

{-|
A @'ThreadId' &#x3B1;@ is an abstract type representing a handle to a thread
that is executing or has executed a computation of type @'IO' &#x3B1;@.

@'ThreadId' &#x3B1;@ is an instance of 'Eq', 'Ord' and 'Show', where the 'Ord'
instance implements an arbitrary total ordering over 'ThreadId's. The 'Show'
instance lets you convert an arbitrary-valued 'ThreadId' to string form; showing
a 'ThreadId' value is occasionally useful when debugging or diagnosing the
behaviour of a concurrent program.
-}
data ThreadId α = ThreadId
    { stopped  ∷ Broadcast (Either SomeException α)
    , threadId ∷ Conc.ThreadId -- ^ Extract the underlying 'Conc.ThreadId'
                               -- (@Control.Concurrent.ThreadId@).
    } deriving Typeable

instance Eq (ThreadId α) where
    (==) = (==) `on` threadId

instance Ord (ThreadId α) where
    compare = compare `on` threadId

instance Show (ThreadId α) where
    show = show ∘ threadId


-------------------------------------------------------------------------------
-- * Forking threads
-------------------------------------------------------------------------------

{-|
Sparks off a new thread to run the given 'IO' computation and returns the
'ThreadId' of the newly created thread.

The new thread will be a lightweight thread; if you want to use a foreign
library that uses thread-local storage, use 'forkOS' instead.

GHC note: the new thread inherits the blocked state of the parent (see
'Control.Exception.block').

The newly created thread has an exception handler that discards the exceptions
'BlockedIndefinitelyOnMVar', 'BlockedIndefinitelyOnSTM', and 'ThreadKilled'. All
other exceptions are recorded in the 'ThreadId' and can be retrieved using
'wait'.
-}
forkIO ∷ IO α → IO (ThreadId α)
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
forkOS ∷ IO α → IO (ThreadId α)
forkOS = fork Conc.forkOS

{-|
Internally used function which generalises 'forkIO' and 'forkOS'. Parametrised
by the function which does the actual forking.
-}
fork ∷ (IO () → IO Conc.ThreadId) → IO α → IO (ThreadId α)
fork doFork a = do
  stop ← Broadcast.new
  let broadcastToStop = broadcast stop
  tid ← ifM blocked (        doFork $ try          a  >>= broadcastToStop)
                    (block $ doFork $ try (unblock a) >>= broadcastToStop)
  return $ ThreadId stop tid


-------------------------------------------------------------------------------
-- * Waiting on threads
-------------------------------------------------------------------------------

{-|
Block until the given thread is terminated.

* Returns @'Right' x@ if the thread terminated normally and returned @x@.

* Returns @'Left' e@ if some exception @e@ was thrown in the thread and wasn't
caught.
-}
wait ∷ ThreadId α → IO (Either SomeException α)
wait = listen ∘ stopped

-- | Like 'wait' but will ignore the value returned by the thread.
wait_ ∷ ThreadId α → IO ()
wait_ = void ∘ wait

-- | Like 'wait' but will either rethrow the exception that was thrown in the
-- thread or return the value that was returned by the thread.
unsafeWait ∷ ThreadId α → IO α
unsafeWait tid = wait tid >>= either throwInner return

-- | Like 'unsafeWait' in that it will rethrow the exception that was thrown in
-- the thread but it will ignore the value returned by the thread.
unsafeWait_ ∷ ThreadId α → IO ()
unsafeWait_ tid = wait tid >>= either throwInner (const $ return ())


-- ** Waiting with a timeout

{-|
Block until the given thread is terminated or until a timer expires.

* Returns 'Nothing' if a timeout occurred.

* Returns 'Just' the result 'wait' would return when the thread finished within
the specified time.

The timeout is specified in microseconds.
-}
waitTimeout ∷ ThreadId α → Integer → IO (Maybe (Either SomeException α))
waitTimeout = listenTimeout ∘ stopped

-- | Like 'waitTimeout' but will ignore the value returned by the thread.
-- Returns 'False' when a timeout occurred and 'True' otherwise.
waitTimeout_ ∷ ThreadId α → Integer → IO Bool
waitTimeout_ tid t = isJust <$> waitTimeout tid t

{-|
Like 'waitTimeout' but will rethrow the exception that was thrown in the
thread. Returns 'Nothing' if a timeout occured or 'Just' the value returned from
the target thread.
-}
unsafeWaitTimeout ∷ ThreadId α → Integer → IO (Maybe α)
unsafeWaitTimeout tid t = waitTimeout tid t >>=
                            maybe (return Nothing)
                                  (either throwInner
                                          (return ∘ Just))

-- | Like 'unsafeWaitTimeout' in that it will rethrow the exception that was
-- thrown in the thread but it will ignore the value returned by the thread.
-- Returns 'False' when a timeout occurred and 'True' otherwise.
unsafeWaitTimeout_ ∷ ThreadId α → Integer → IO Bool
unsafeWaitTimeout_ tid t = waitTimeout tid t >>=
                             maybe (return False)
                                   (either throwInner
                                           (const $ return True))


-------------------------------------------------------------------------------
-- * Quering thread status
-------------------------------------------------------------------------------

{-|
Returns 'True' if the given thread is currently running.

Notice that this observation is only a snapshot of a thread's state. By the time
a program reacts on its result it may already be out of date.
-}
isRunning ∷ ThreadId α → IO Bool
isRunning = fmap isNothing ∘ tryListen ∘ stopped


-------------------------------------------------------------------------------
-- * Convenience functions
-------------------------------------------------------------------------------

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
throwTo ∷ Exception e ⇒ ThreadId α → e → IO ()
throwTo = Conc.throwTo ∘ threadId

{-|
'killThread' terminates the given thread (GHC only). Any work already done by
the thread isn't lost: the computation is suspended until required by another
thread. The memory used by the thread will be garbage collected if it isn't
referenced from anywhere. The 'killThread' function is defined in terms of
'throwTo'.

This function blocks until the target thread is terminated. It is a no-op if the
target thread has already completed.
-}
killThread ∷ ThreadId α → IO ()
killThread tid = throwTo tid ThreadKilled >> wait_ tid

{-|
Like 'killThread' but with a timeout. Returns 'True' if the target thread was
terminated within the given amount of time, 'False' otherwise.

The timeout is specified in microseconds.

Note that even when a timeout occurs, the target thread can still terminate at a
later time as a direct result of calling this function.
-}
killThreadTimeout ∷ ThreadId α → Integer → IO Bool
killThreadTimeout tid time = throwTo tid ThreadKilled >> waitTimeout_ tid time


-------------------------------------------------------------------------------
-- * Thread groups
-------------------------------------------------------------------------------

newtype Group α = Group (MVar [ThreadId α])

newGroup ∷ IO (Group α)
newGroup = Group <$> newMVar []

forkIOInGroup ∷ Group α → IO α → IO (ThreadId α)
forkIOInGroup = forkInGroup Conc.forkIO

forkOSInGroup ∷ Group α → IO α → IO (ThreadId α)
forkOSInGroup = forkInGroup Conc.forkOS

forkInGroup ∷ (IO () → IO Conc.ThreadId) → Group α → IO α → IO (ThreadId α)
forkInGroup doFork (Group mv) act = do
  stop ← Broadcast.new
  let frk a = do tids ← takeMVar mv
                 nativeTid ← doFork $ try a >>= \r → do deleteMyTid
                                                        broadcast stop r
                 let tid = ThreadId stop nativeTid
                 putMVar mv $ tid:tids
                 return tid
  ifM blocked
      (frk act)
      (block $ frk $ unblock act)
  where
    deleteMyTid = do
      myNativeTid ← myThreadId
      tids ← takeMVar mv
      putMVar mv $! deleteWhen' ((myNativeTid ≡) ∘ threadId) tids

waitAll ∷ Group α → IO ()
waitAll (Group mv) = readMVar mv >>= mapM_ wait_


-- The End ---------------------------------------------------------------------
