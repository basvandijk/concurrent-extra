{-# LANGUAGE BangPatterns
           , DeriveDataTypeable
           , NoImplicitPrelude
           , UnicodeSyntax
  #-}

--------------------------------------------------------------------------------
-- |
-- Module     : Control.Concurrent.RLock
-- Copyright  : (c) 2010 Bas van Dijk & Roel van Dijk
-- License    : BSD3 (see the file LICENSE)
-- Maintainer : Bas van Dijk <v.dijk.bas@gmail.com>
--            , Roel van Dijk <vandijk.roel@gmail.com>
--
-- This module provides the 'RLock' synchronization mechanism. It was inspired
-- by the Python @RLock@ and Java @ReentrantLock@ objects and should behave in a
-- similar way. See:
--
-- <http://docs.python.org/3.1/library/threading.html#rlock-objects>
--
-- and:
--
-- <http://java.sun.com/javase/7/docs/api/java/util/concurrent/locks/ReentrantLock.html>
--
-- All functions are /exception safe/. Throwing asynchronous exceptions will not
-- compromise the internal state of a 'RLock'.
--
-- This module is intended to be imported qualified. We suggest importing it like:
--
-- @
-- import           Control.Concurrent.RLock          ( RLock )
-- import qualified Control.Concurrent.RLock as RLock ( ... )
-- @
--
--------------------------------------------------------------------------------

module Control.Concurrent.RLock
    ( RLock
      -- * Creating reentrant locks
    , new
    , newAcquired
      -- * Locking and unlocking
    , acquire
    , tryAcquire
    , release
      -- * Convenience functions
    , with
    , tryWith
    , wait
      -- * Querying reentrant locks
    , State
    , state
    ) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Control.Applicative     ( (<$>), liftA2 )
import Control.Concurrent      ( ThreadId, myThreadId )
import Control.Concurrent.MVar ( MVar, newMVar, takeMVar, readMVar, putMVar )
import Control.Exception       ( block, bracket_, finally )
import Control.Monad           ( Monad, return, (>>=), fail, (>>), fmap )
import Data.Bool               ( Bool(False, True), otherwise )
import Data.Eq                 ( Eq )
import Data.Function           ( ($) )
import Data.Maybe              ( Maybe(Nothing, Just) )
import Data.Tuple              ( fst )
import Data.Typeable           ( Typeable )
import Prelude                 ( Integer, fromInteger, succ, pred, error )
import System.IO               ( IO )

-- from base-unicode-symbols
import Data.Eq.Unicode         ( (≡) )
import Data.Function.Unicode   ( (∘) )
import Data.Monoid.Unicode     ( (⊕) )

-- from ourselves:
import           Control.Concurrent.Lock ( Lock )
import qualified Control.Concurrent.Lock as Lock
    ( new, newAcquired, acquire, release, wait )


--------------------------------------------------------------------------------
-- Reentrant locks
--------------------------------------------------------------------------------

{-| A reentrant lock is in one of two states: \"Locked\" or \"Unlocked\". When
the lock is in the \"Locked\" state it has two additional properties:

* Its /owner/: the thread that acquired the lock.

* Its /acquired count/: how many times its owner acquired the lock.
-}
newtype RLock = RLock {un ∷ MVar (State, Lock)}
    deriving (Eq, Typeable)

{-| The state of an 'RLock'.

* 'Nothing' indicates an \"Unlocked\" state.

* @'Just' (tid, n)@ indicates a \"Locked\" state where the thread identified by @tid@
acquired the lock @n@ times.
-}
type State = Maybe (ThreadId, Integer)

-- | Create a reentrant lock in the \"Unlocked\" state.
new ∷ IO RLock
new = do lock ← Lock.new
         RLock <$> newMVar (Nothing, lock)

-- | Create a reentrant lock in the \"Locked\" state (with the current thread as
-- owner and an acquired count of 1).
newAcquired ∷ IO RLock
newAcquired = do myTID ← myThreadId
                 lock ← Lock.newAcquired
                 RLock <$> newMVar (Just (myTID, 1), lock)

{-| @acquire@ behaves as follows:

* When the state is \"Unlocked\", @acquire@ changes the state to \"Locked\"
(with the current thread as owner and an acquired count of 1) and returns
immediately.

* When the state is \"Locked\" and the current thread owns the lock, @acquire@
keeps the state to \"Locked\" but increments the acquired count and returns
immediately.

* When the state is \"Locked\" and the current thread does not own the lock,
@acquire@ /blocks/ until the owner releases the lock. @acquire@ then changes the
state to \"Locked\" (with the current thread as owner and an acquired count of
1).

There are two further important properties of @acquire@:

* @acquire@ is single-wakeup. That is, if there are multiple threads blocked on
@acquire@, and the lock is released, only one thread will be woken up. The
runtime guarantees that the woken thread completes its @acquire@ operation.

* When multiple threads are blocked on @acquire@, they are woken up in FIFO
order. This is useful for providing fairness properties of abstractions built
using locks. (Note that this differs from the Python implementation where the
wake-up order is undefined)
-}
acquire ∷ RLock → IO ()
acquire (RLock mv) = do
  myTID ← myThreadId
  block $ let acq = do t@(mb, lock) ← takeMVar mv
                       case mb of
                         Nothing         → do Lock.acquire lock
                                              putMVar mv (Just (myTID, 1), lock)
                         Just (tid, n)
                           | myTID ≡ tid → let !sn = succ n
                                           in putMVar mv (Just (tid, sn), lock)
                           | otherwise   → do putMVar mv t
                                              Lock.wait lock
                                              acq
          in acq

{-| A non-blocking 'acquire'.

* When the state is \"Unlocked\", @tryAcquire@ changes the state to \"Locked\"
(with the current thread as owner and an acquired count of 1) and returns
immediately with 'True'.

* When the state is \"Locked\", @tryAcquire@ leaves the state unchanged and
returns immediately with 'False'.
-}
tryAcquire ∷ RLock → IO Bool
tryAcquire (RLock mv) = do
  myTID ← myThreadId
  block $ do
    t@(mb, lock) ← takeMVar mv
    case mb of
      Nothing         → do Lock.acquire lock
                           putMVar mv (Just (myTID, 1), lock)
                           return True
      Just (tid, n)
        | myTID ≡ tid → do let !sn = succ n
                           putMVar mv (Just (tid, sn), lock)
                           return True

        | otherwise   → do putMVar mv t
                           return False

{-| @release@ decrements the acquired count. When a lock is released with an
acquired count of 1 its state is changed to \"Unlocked\".

Note that it is both an error to release a lock in the \"Unlocked\" state and to
release a lock that is not owned by the current thread.

If there are any threads blocked on 'acquire' the thread that first called
@acquire@ will be woken up.
-}
release ∷ RLock → IO ()
release (RLock mv) = do
  myTID ← myThreadId
  block $ do
    t@(mb, lock) ← takeMVar mv
    let err msg = do putMVar mv t
                     error $ "Control.Concurrent.RLock.release: " ⊕ msg
    case mb of
      Nothing → err "Can't release an unacquired RLock!"
      Just (tid, n)
        | myTID ≡ tid → if n ≡ 1
                        then do Lock.release lock
                                putMVar mv (Nothing, lock)
                        else let !pn = pred n
                             in putMVar mv (Just (tid, pn), lock)
        | otherwise → err "Calling thread does not own the RLock!"

{-| A convenience function which first acquires the lock and then
performs the computation. When the computation terminates, whether
normally or by raising an exception, the lock is released.

Note that: @with = 'liftA2' 'bracket_' 'acquire' 'release'@.
-}
with ∷ RLock → IO α → IO α
with = liftA2 bracket_ acquire release

{-| A non-blocking 'with'. @tryWith@ is a convenience function which first tries
to acquire the lock. If that fails, 'Nothing' is returned. If it succeeds, the
computation is performed. When the computation terminates, whether normally or
by raising an exception, the lock is released and 'Just' the result of the
computation is returned.
-}
tryWith ∷ RLock → IO α → IO (Maybe α)
tryWith l a = block $ do
  acquired ← tryAcquire l
  if acquired
    then Just <$> a `finally` release l
    else return Nothing

{-|
* When the state is \"Locked\", @wait@ /blocks/ until a call to 'release' in
another thread changes it to \"Unlocked\".

* When the state is \"Unlocked\" @wait@ returns immediately.

@wait@ does not alter the state of the lock.

Note that @wait@ is just a convenience function defined as:

@wait l = 'block' '$' 'acquire' l '>>' 'release' l@
-}
wait ∷ RLock → IO ()
wait l = block $ acquire l >> release l

{-| Determine the state of the reentrant lock.

Notice that this is only a snapshot of the state. By the time a program reacts
on its result it may already be out of date.
-}
state ∷ RLock → IO State
state = fmap fst ∘ readMVar ∘ un


-- The End ---------------------------------------------------------------------
