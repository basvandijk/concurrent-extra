{-# LANGUAGE CPP, DeriveDataTypeable, NoImplicitPrelude, UnicodeSyntax #-}

#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Trustworthy #-}
#endif

--------------------------------------------------------------------------------
-- |
-- Module     : Control.Concurrent.STM.Lock
-- Copyright  : (c) 2010-2011 Bas van Dijk & Roel van Dijk
-- License    : BSD3 (see the file LICENSE)
-- Maintainer : Bas van Dijk <v.dijk.bas@gmail.com>
--            , Roel van Dijk <vandijk.roel@gmail.com>
--
-- This module provides an 'STM' version of @Control.Concurrent.Lock@.
--
-- This module is intended to be imported qualified. We suggest importing it like:
--
-- @
-- import           Control.Concurrent.STM.Lock         ( Lock )
-- import qualified Control.Concurrent.STM.Lock as Lock ( ... )
-- @
--
--------------------------------------------------------------------------------

module Control.Concurrent.STM.Lock
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
    , wait

      -- * Querying locks
    , locked
    ) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Control.Applicative          ( liftA2 )
import Control.Exception            ( bracket_, onException )
import Control.Monad                ( Monad, return, (>>), when )
import Data.Bool                    ( Bool, not )

#ifdef __HADDOCK__
import Data.Bool                    ( Bool(False, True) )
#endif

import Data.Eq                      ( Eq )
import Data.Function                ( ($) )
import Data.Functor                 ( fmap, (<$>) )
import Data.Maybe                   ( Maybe(Nothing, Just), isJust )
import Data.Typeable                ( Typeable )
import Prelude                      ( error )
import System.IO                    ( IO )

#if __GLASGOW_HASKELL__ < 700
import Control.Monad                ( (>>=), fail )
#endif

-- from stm:
import Control.Concurrent.STM       ( STM, atomically )

#ifdef __HADDOCK__
import Control.Concurrent.STM       ( retry )
#endif

import Control.Concurrent.STM.TMVar ( TMVar, newTMVar, newEmptyTMVar
                                    , takeTMVar, tryTakeTMVar
                                    , putTMVar, tryPutTMVar
                                    , isEmptyTMVar
                                    )

-- from base-unicode-symbols:
import Data.Function.Unicode        ( (∘) )

-- from concurrent-extra (this package):
import Utils                        ( mask )


--------------------------------------------------------------------------------
-- Locks
--------------------------------------------------------------------------------

-- | A lock is in one of two states: \"locked\" or \"unlocked\".
newtype Lock = Lock {un ∷ TMVar ()}
    deriving (Typeable, Eq)


--------------------------------------------------------------------------------
-- Creating locks
--------------------------------------------------------------------------------

-- | Create a lock in the \"unlocked\" state.
new ∷ STM Lock
new = Lock <$> newTMVar ()

-- | Create a lock in the \"locked\" state.
newAcquired ∷ STM Lock
newAcquired = Lock <$> newEmptyTMVar


--------------------------------------------------------------------------------
-- Locking and unlocking
--------------------------------------------------------------------------------

{-|
* When the state is \"locked\" @acquire@ will 'retry' the transaction.

* When the state is \"unlocked\" @acquire@ will change the state to \"locked\".
-}
acquire ∷ Lock → STM ()
acquire = takeTMVar ∘ un

{-|
A non-blocking 'acquire'.

* When the state is \"unlocked\" @tryAcquire@ changes the state to \"locked\"
and returns 'True'.

* When the state is \"locked\" @tryAcquire@ leaves the state unchanged and
returns 'False'.
-}
tryAcquire ∷ Lock → STM Bool
tryAcquire = fmap isJust ∘ tryTakeTMVar ∘ un

{-|
@release@ changes the state to \"unlocked\" and returns immediately.

Note that it is an error to release a lock in the \"unlocked\" state!
-}
release ∷ Lock → STM ()
release (Lock tmv) = do
  b ← tryPutTMVar tmv ()
  when (not b) $ error "Control.Concurrent.STM.Lock.release: Can't release unlocked Lock!"


--------------------------------------------------------------------------------
-- Convenience functions
--------------------------------------------------------------------------------

{-|
A convenience function which first acquires the lock and then performs the
computation. When the computation terminates, whether normally or by raising an
exception, the lock is released.
-}
with ∷ Lock → IO a → IO a
with = liftA2 bracket_ (atomically ∘ acquire) (atomically ∘ release)

{-|
A non-blocking 'with'. @tryWith@ is a convenience function which first tries to
acquire the lock. If that fails, 'Nothing' is returned. If it succeeds, the
computation is performed. When the computation terminates, whether normally or
by raising an exception, the lock is released and 'Just' the result of the
computation is returned.
-}
tryWith ∷ Lock → IO α → IO (Maybe α)
tryWith l a = mask $ \restore → do
  acquired ← atomically (tryAcquire l)
  if acquired
    then do r ← restore a `onException` atomically (release l)
            atomically (release l)
            return $ Just r
    else return Nothing

{-|
* When the state is \"locked\", @wait@ will 'retry' the transaction

* When the state is \"unlocked\" @wait@ returns immediately.

@wait@ does not alter the state of the lock.

Note that @wait@ is just a convenience function which can be defined as:

@wait l = 'acquire' l '>>' 'release' l@
-}
wait ∷ Lock → STM ()
wait (Lock tmv) = takeTMVar tmv >> putTMVar tmv ()


--------------------------------------------------------------------------------
-- Querying locks
--------------------------------------------------------------------------------

{-|
Determines if the lock is in the \"locked\" state.

Note that this is only a snapshot of the state. By the time a program reacts
on its result it may already be out of date.
-}
locked ∷ Lock → STM Bool
locked = isEmptyTMVar ∘ un
