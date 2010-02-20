{-# LANGUAGE DeriveDataTypeable, NoImplicitPrelude, UnicodeSyntax #-}

--------------------------------------------------------------------------------
-- |
-- Module     : Control.Concurrent.Lock
-- Copyright  : (c) 2010 Bas van Dijk & Roel van Dijk
-- License    : BSD3 (see the file LICENSE)
-- Maintainer : Bas van Dijk <v.dijk.bas@gmail.com>
--            , Roel van Dijk <vandijk.roel@gmail.com>
--
-- This module provides the 'Lock' synchronization mechanism. It was inspired by
-- the Python and Java @Lock@ objects and should behave in a similar way. See:
--
-- <http://docs.python.org/3.1/library/threading.html#lock-objects>
--
-- and:
--
-- <http://java.sun.com/javase/7/docs/api/java/util/concurrent/locks/Lock.html>
--
-- All functions are /exception safe/. Throwing asynchronous exceptions will not
-- compromise the internal state of a 'Lock'.
--
-- This module is intended to be imported qualified. We suggest importing it like:
--
-- @
-- import           Control.Concurrent.Lock         ( Lock )
-- import qualified Control.Concurrent.Lock as Lock ( ... )
-- @
--
--------------------------------------------------------------------------------

module Control.Concurrent.Lock
    ( Lock
      -- * Creating locks
    , new
    , newAcquired
      -- * Locking and unlocking
    , acquire
    , tryAcquire
    , release
      -- * Convenience functions
    , with
    , tryWith
      -- * Querying locks
    , locked
    ) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base
import Control.Applicative     ( (<$>), liftA2 )
import Control.Concurrent.MVar ( MVar, newMVar, newEmptyMVar
                               , takeMVar, tryTakeMVar
                               , tryPutMVar
                               , isEmptyMVar
                               )
import Control.Exception       ( block, bracket_, finally )
import Control.Monad           ( Monad, return, (>>=), fail, when, fmap )
import Data.Bool               ( Bool, not )
import Data.Eq                 ( Eq )
import Data.Function           ( ($) )
import Data.Maybe              ( Maybe(Nothing, Just), isJust )
import Data.Typeable           ( Typeable )
import Prelude                 ( error )
import System.IO               ( IO )

-- from base-unicode-symbols
import Data.Function.Unicode   ( (∘) )


--------------------------------------------------------------------------------
-- Locks
--------------------------------------------------------------------------------

{-| A lock is in one of two states, \"locked\" or \"unlocked\". -}
newtype Lock = Lock {un ∷ MVar ()} deriving (Eq, Typeable)

-- | Create an unlocked lock.
new ∷ IO Lock
new = Lock <$> newMVar ()

-- | Create a locked lock.
newAcquired ∷ IO Lock
newAcquired = Lock <$> newEmptyMVar

{-| When the state is unlocked, @acquire@ changes the state to locked and
returns immediately. When the state is locked, @acquire@ blocks until a call to
'release' in another thread changes it to unlocked, then the @acquire@ call
resets it to locked and returns.

There are two further important properties of @acquire@:

* @acquire@ is single-wakeup. That is, if there are multiple threads blocked on
@acquire@, and the lock is released, only one thread will be woken up. The
runtime guarantees that the woken thread completes its @acquire@ operation.

* When multiple threads are blocked on @acquire@, they are woken up in FIFO
order. This is useful for providing fairness properties of abstractions built
using locks. (Note that this differs from the Python implementation where the
wake-up order is undefined)
-}
acquire ∷ Lock → IO ()
acquire = takeMVar ∘ un

{-| A non-blocking 'acquire'. When the state is unlocked, @tryAcquire@ changes
the state to locked and returns immediately with 'True'. When the state is
locked, @tryAcquire@ leaves the state unchanged and returns immediately with
'False'.
-}
tryAcquire ∷ Lock → IO Bool
tryAcquire = fmap isJust ∘ tryTakeMVar ∘ un

{-| @release@ changes the state to unlocked and returns immediately.

Note that it is an error to release an unlocked lock!

If there are any threads blocked on 'acquire' the thread that first called
@acquire@ will be woken up.
-}
release ∷ Lock → IO ()
release (Lock mv) = do
  b ← tryPutMVar mv ()
  when (not b) $ error "Control.Concurrent.Lock.release: Can't release unlocked Lock!"

{-| A convenience function which first acquires the lock and then
performs the computation. When the computation terminates, whether
normally or by raising an exception, the lock is released.

Note that: @with = 'liftA2' 'bracket_' 'acquire' 'release'@.
-}
with ∷ Lock → IO a → IO a
with = liftA2 bracket_ acquire release

{-| A non-blocking 'with'. @tryWith@ is a convenience function which first tries
to acquire the lock. If that fails, 'Nothing' is returned. If it succeeds, the
computation is performed. When the computation terminates, whether normally or
by raising an exception, the lock is released and 'Just' the result of the
computation is returned.
-}
tryWith ∷ Lock → IO α → IO (Maybe α)
tryWith l a = block $ do
  acquired ← tryAcquire l
  if acquired
    then Just <$> a `finally` release l
    else return Nothing

-- | Determines if the lock is in the locked state.
--
-- Notice that this is only a snapshot of the state. By the time a program
-- reacts on its result it may already be out of date.
locked ∷ Lock → IO Bool
locked = isEmptyMVar ∘ un


-- The End ---------------------------------------------------------------------
