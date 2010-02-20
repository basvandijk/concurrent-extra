{-# LANGUAGE DeriveDataTypeable
           , NamedFieldPuns
           , NoImplicitPrelude
           , TupleSections
           , UnicodeSyntax
  #-}

-------------------------------------------------------------------------------
-- |
-- Module     : Control.Concurrent.ReadWriteLock
-- Copyright  : (c) 2010 Bas van Dijk & Roel van Dijk
-- License    : BSD3 (see the file LICENSE)
-- Maintainer : Bas van Dijk <v.dijk.bas@gmail.com>
--            , Roel van Dijk <vandijk.roel@gmail.com>
--
-- Multiple-reader, single-writer locks. Used to protect shared resources which
-- may be concurrently read, but only sequentially written.
--
-- All functions are /exception safe/. Throwing asynchronous exceptions will not
-- compromise the internal state of an 'RWLock'. This means it is perfectly safe
-- to kill a thread that is blocking on, for example, 'acquireRead'.
--
-- See also Java's version:
-- <http://java.sun.com/javase/7/docs/api/java/util/concurrent/locks/ReadWriteLock.html>
--
-- This module is designed to be imported qualified. We suggest importing it
-- like:
--
-- @
-- import           Control.Concurrent.ReadWriteLock        ( RWLock )
-- import qualified Control.Concurrent.ReadWriteLock as RWL ( ... )
-- @
--
-------------------------------------------------------------------------------

module Control.Concurrent.ReadWriteLock
  ( RWLock

    -- *Creating Read-Write Locks
  , new
  , newAcquiredRead
  , newAcquiredWrite

    -- *Read access
    -- **Blocking
  , acquireRead
  , releaseRead
  , withRead
    -- **Non-blocking
  , tryAcquireRead
  , tryWithRead
    -- *Write access
    -- **Blocking
  , acquireWrite
  , releaseWrite
  , withWrite
    -- **Non-blocking
  , tryAcquireWrite
  , tryWithWrite
  ) where


-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

-- from base
import Control.Applicative     ( (<$>), liftA2 )
import Control.Concurrent.MVar ( MVar, newMVar, takeMVar, putMVar
                               , modifyMVar_, swapMVar
                               )
import Control.Exception       ( block, bracket_, finally )
import Control.Monad           ( return, (>>=), return, fail, (>>)
                               , when, liftM3
                               )
import Data.Bool               ( Bool(False, True) )
import Data.Char               ( String )
import Data.Eq                 ( Eq, (==) )
import Data.Function           ( ($), const )
import Data.Int                ( Int )
import Data.Maybe              ( Maybe(Nothing, Just) )
import Data.Typeable           ( Typeable )
import Prelude                 ( fromInteger, succ, pred
                               , ($!), error
                               )
import System.IO               ( IO )

-- from base-unicode-symbols
import Data.Function.Unicode   ( (∘) )
import Data.Monoid.Unicode     ( (⊕) )

-- from concurrent-extra
import           Control.Concurrent.Lock ( Lock )
import qualified Control.Concurrent.Lock as Lock

import Utils ( void )

-------------------------------------------------------------------------------
-- Read Write Lock
-------------------------------------------------------------------------------

{-| Multiple-reader, single-writer lock. Is in one of three states:

* \"Free\": Read or write access can be acquired without blocking.

* \"Read\": One or more threads have acquired read access. Blocks write access.

* \"Write\": A single thread has acquired write access. Blocks other threads
from acquiring both read and write access.
-}
data RWLock = RWLock { state     ∷ MVar State
                     , readLock  ∷ Lock
                     , writeLock ∷ Lock
                     } deriving (Eq, Typeable)

-- | Internal state of the 'RWLock'.
data State = Free | Read Int | Write

{-| Create a new 'RWLock'. The initial state is \"free\"; either read or write
access can be acquired without blocking.
-}
new ∷ IO RWLock
new = liftM3 RWLock (newMVar Free)
                    Lock.new
                    Lock.new

{-| Create a new 'RWLock' in the \"Read\" state; only read can be acquired
without blocking.
-}
newAcquiredRead ∷ IO RWLock
newAcquiredRead = liftM3 RWLock (newMVar $ Read 1)
                                Lock.newAcquired
                                Lock.new

{-| Create a new 'RWLock' in the \"Write\" state; either acquiring read or write
will block.
-}
newAcquiredWrite ∷ IO RWLock
newAcquiredWrite = liftM3 RWLock (newMVar Write)
                                 Lock.new
                                 Lock.newAcquired

{-| Acquire the read lock.

Blocks if another thread has acquired write access. If @acquireRead@ terminates
without throwing an exception the state of the 'RWLock' will be \"read\".

Implementation note: Throws an exception when more than (maxBound :: Int)
simultaneous threads acquire the read lock. But that is unlikely.
-}
acquireRead ∷ RWLock → IO ()
acquireRead (RWLock {state, readLock, writeLock}) = block $ do
  st ← takeMVar state
  case st of
    Free   → do Lock.acquire readLock
                putMVar state (Read 1)
    Read n → putMVar state (Read ∘ succ $! n)
    Write  → do putMVar state st
                Lock.acquire writeLock
                modifyMVar_ state ∘ const $ do
                  Lock.acquire readLock
                  return $ Read 1


{-| Try to acquire the read lock; non blocking.

Like 'acquireRead', but doesn't block. Returns 'True' if the resulting state is
\"read\", 'False' otherwise.
-}
tryAcquireRead ∷ RWLock → IO Bool
tryAcquireRead (RWLock {state, readLock}) = block $ do
  st ← takeMVar state
  case st of
    Free   → do Lock.acquire readLock
                putMVar state (Read 1)
                return True
    Read n → do putMVar state (Read ∘ succ $! n)
                return True
    Write  → do putMVar state st
                return False

{-| Release the read lock.

If the calling thread was the last one to relinquish read access the state will
revert to \"free\".

It is an error to release read access to an 'RWLock' which is not in the
\"read\" state.
-}
releaseRead ∷ RWLock → IO ()
releaseRead (RWLock {state, readLock}) = block $ do
  st ← takeMVar state
  case st of
    Free   → putMVar state st >> err
    Read 1 → do Lock.release readLock
                putMVar state Free
    Read n → putMVar state (Read ∘ pred $! n)
    Write  → putMVar state st >> err
  where
    err = error $ moduleName ⊕ ".releaseRead: already released"

{-| A convenience function wich first acquires read access and then performs the
computation. When the computation terminates, whether normally or by raising an
exception, the read lock is released.
-}
withRead ∷ RWLock → IO α → IO α
withRead = liftA2 bracket_ acquireRead releaseRead

{-| A non-blocking 'withRead'. First tries to acquire the lock. If that fails,
'Nothing' is returned. If it succeeds, the computation is performed. When the
computation terminates, whether normally or by raising an exception, the lock is
released and 'Just' the result of the computation is returned.
-}
tryWithRead ∷ RWLock → IO α → IO (Maybe α)
tryWithRead l a = block $ do
  acquired ← tryAcquireRead l
  if acquired
    then Just <$> a `finally` releaseRead l
    else return Nothing

{-| Acquire the write lock.

Blocks if another thread has acquired either read or write access. If
@acquireWrite@ terminates without throwing an exception the state of the
'RWLock' will be \"write\".
-}
acquireWrite ∷ RWLock → IO ()
acquireWrite (RWLock {state, readLock, writeLock}) = block $ do
  st ← takeMVar state
  case st of
    Free   → do Lock.acquire writeLock
                putMVar state Write
    Read _ → do putMVar state st
                Lock.acquire readLock
                modifyMVar_ state ∘ const $ do
                  Lock.acquire writeLock
                  return Write
    Write  → do putMVar state st
                Lock.acquire writeLock
                void $ swapMVar state Write

{-| Try to acquire the write lock; non blocking.

Like 'acquireWrite', but doesn't block. Returns 'True' if the resulting state is
\"write\", 'False' otherwise.
-}
tryAcquireWrite ∷ RWLock → IO Bool
tryAcquireWrite (RWLock {state, writeLock}) = block $ do
  st ← takeMVar state
  case st of
    Free   → do Lock.acquire writeLock
                putMVar state Write
                return True
    Read _ → do putMVar state st
                return False
    Write  → do putMVar state st
                b ← Lock.tryAcquire writeLock
                when b ∘ void $ swapMVar state Write
                return b

{-| Release the write lock.

If @releaseWrite@ terminates without throwing an exception the state will be
\"free\".

It is an error to release write access to an 'RWLock' which is not in the
\"write\" state.
-}
releaseWrite ∷ RWLock → IO ()
releaseWrite (RWLock {state, writeLock}) = block $ do
  st ← takeMVar state
  case st of
    Free   → putMVar state st >> err
    Read _ → putMVar state st >> err
    Write  → do Lock.release writeLock
                putMVar state Free
  where
    err = error $ moduleName ⊕ ".releaseWrite: already released"

{-| A convenience function wich first acquires write access and then performs
the computation. When the computation terminates, whether normally or by raising
an exception, the write lock is released.
-}
withWrite ∷ RWLock → IO α → IO α
withWrite = liftA2 bracket_ acquireWrite releaseWrite

{-| A non-blocking 'withWrite'. First tries to acquire the lock. If that fails,
'Nothing' is returned. If it succeeds, the computation is performed. When the
computation terminates, whether normally or by raising an exception, the lock is
released and 'Just' the result of the computation is returned.
-}
tryWithWrite ∷ RWLock → IO α → IO (Maybe α)
tryWithWrite l a = block $ do
  acquired ← tryAcquireWrite l
  if acquired
    then Just <$> a `finally` releaseWrite l
    else return Nothing

moduleName ∷ String
moduleName = "Control.Concurrent.ReadWriteLock"


-- The End --------------------------------------------------------------------
