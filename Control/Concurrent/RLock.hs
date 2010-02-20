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
      -- * Querying reentrant locks
    , recursionLevel
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
import Data.Maybe              ( Maybe(Nothing, Just), maybe )
import Data.Tuple              ( fst, snd )
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
    ( new, newAcquired, acquire, release )


--------------------------------------------------------------------------------
-- Reentrant locks
--------------------------------------------------------------------------------

newtype RLock = RLock {un ∷ MVar (Maybe (ThreadId, Integer), Lock)}
    deriving (Eq, Typeable)

new ∷ IO RLock
new = do lock ← Lock.new
         RLock <$> newMVar (Nothing, lock)

newAcquired ∷ IO RLock
newAcquired = do myTID ← myThreadId
                 lock ← Lock.newAcquired
                 RLock <$> newMVar (Just (myTID, 1), lock)

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
                                              Lock.acquire lock
                                              Lock.release lock
                                              acq
          in acq

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

with ∷ RLock → IO α → IO α
with = liftA2 bracket_ acquire release

tryWith ∷ RLock → IO α → IO (Maybe α)
tryWith l a = block $ do
  acquired ← tryAcquire l
  if acquired
    then Just <$> a `finally` release l
    else return Nothing

recursionLevel ∷ RLock → IO Integer
recursionLevel = fmap (maybe 0 snd ∘ fst) ∘ readMVar ∘ un


-- The End ---------------------------------------------------------------------
