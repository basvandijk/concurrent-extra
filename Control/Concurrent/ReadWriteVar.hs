{-# LANGUAGE CPP
           , DeriveDataTypeable
           , NoImplicitPrelude
           , TupleSections
           , UnicodeSyntax
  #-}

-------------------------------------------------------------------------------
-- |
-- Module     : Control.Concurrent.ReadWriteVar
-- Copyright  : 2010—2011 Bas van Dijk & Roel van Dijk
-- License    : BSD3 (see the file LICENSE)
-- Maintainer : Bas van Dijk <v.dijk.bas@gmail.com>
--            , Roel van Dijk <vandijk.roel@gmail.com>
--
-- Concurrent read, sequential write variables. Comparable to an 'IORef' with
-- more advanced synchronization mechanisms. The value stored inside the 'RWVar'
-- can be read and used by multiple threads at the same time. Concurrent
-- computations inside a 'with' \"block\" observe the same value.
--
-- Observing and changing the contents of an 'RWVar' are mutually
-- exclusive. The 'with' function will block if 'modify' is active and
-- vice-versa. Furthermore 'with' is fully sequential and will also
-- block on concurrent calls of 'modify'.
--
-- The following are guaranteed deadlocks:
--
-- * @'modify_' v '$' 'const' '$' 'with' v '$' 'const' 'undefined'@
--
-- * @'with' v '$' 'const' '$' 'modify_' v '$' 'const' 'undefined'@
--
-- * @'modify_' v '$' 'const' '$' 'modify_' v '$' 'const' 'undefined'@
--
-- All functions are /exception safe/. Throwing asynchronous exceptions will not
-- compromise the internal state of an 'RWVar'. This also means that threads
-- blocking on 'with' or 'modify' and friends can still be unblocked by throwing
-- an asynchronous exception.
--
-- This module is designed to be imported qualified. We suggest importing it
-- like:
--
-- @
-- import           Control.Concurrent.ReadWriteVar        ( RWVar )
-- import qualified Control.Concurrent.ReadWriteVar as RWV ( ... )
-- @
--
-------------------------------------------------------------------------------

module Control.Concurrent.ReadWriteVar
  ( RWVar
  , new
  , with
  , tryWith
  , modify_
  , modify
  , tryModify_
  , tryModify
  ) where


-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

-- from base:
import Control.Applicative ( liftA2 )
import Control.Monad       ( (>>=) )
import Data.Bool           ( Bool(..) )
import Data.Eq             ( Eq, (==) )
import Data.Function       ( ($), on )
import Data.Functor        ( fmap  )
import Data.Maybe          ( Maybe(..), isJust )
import Data.IORef          ( IORef, newIORef, readIORef )
import Data.Typeable       ( Typeable )
import System.IO           ( IO )
#ifdef __HADDOCK__
import Data.Function       ( const )
import Prelude             ( undefined )
#endif

-- from base-unicode-symbols:
import Data.Function.Unicode ( (∘) )

-- from concurrent-extra (this package):
import           Control.Concurrent.ReadWriteLock ( RWLock )
import qualified Control.Concurrent.ReadWriteLock as RWLock

import Utils ( modifyIORefM, modifyIORefM_ )


-------------------------------------------------------------------------------
-- Read-Write Variables: concurrent read, sequential write
-------------------------------------------------------------------------------

-- | Concurrently readable and sequentially writable variable.
data RWVar α = RWVar RWLock (IORef α) deriving Typeable

instance Eq (RWVar α) where
    (==) = (==) `on` rwlock
        where
          rwlock (RWVar rwl _) = rwl

-- | Create a new 'RWVar'.
new ∷ α → IO (RWVar α)
new = liftA2 RWVar RWLock.new ∘ newIORef

{-| Execute an action that operates on the contents of the 'RWVar'.

The action is guaranteed to have a consistent view of the stored value. Any
function that attempts to 'modify' the contents will block until the action is
completed.

If another thread is modifying the contents of the 'RWVar' this function will
block until the other thread finishes its action.
-}
with ∷ RWVar α → (α → IO β) → IO β
with (RWVar l r) f = RWLock.withRead l $ readIORef r >>= f

{-| Like 'with' but doesn't block. Returns 'Just' the result if read access
could be acquired without blocking, 'Nothing' otherwise.
-}
tryWith ∷ RWVar α → (α → IO β) → IO (Maybe β)
tryWith (RWVar l r) f = RWLock.tryWithRead l $ readIORef r >>= f

{-| Modify the contents of an 'RWVar'.

This function needs exclusive write access to the 'RWVar'. Only one thread can
modify an 'RWVar' at the same time. All others will block.
-}
modify_ ∷ RWVar α → (α → IO α) → IO ()
modify_ (RWVar l r) = RWLock.withWrite l ∘ modifyIORefM_ r

{-| Modify the contents of an 'RWVar' and return an additional value.

Like 'modify_', but allows a value to be returned (&#x3b2;) in addition to the
modified value of the 'RWVar'.
-}
modify ∷ RWVar α → (α → IO (α, β)) → IO β
modify (RWVar l r) = RWLock.withWrite l ∘ modifyIORefM r

{-| Attempt to modify the contents of an 'RWVar'.

Like 'modify_', but doesn't block. Returns 'True' if the contents could be
replaced, 'False' otherwise.
-}
tryModify_ ∷ RWVar α → (α → IO α) → IO Bool
tryModify_ (RWVar l r) = fmap isJust ∘ RWLock.tryWithWrite l ∘ modifyIORefM_ r

{-| Attempt to modify the contents of an 'RWVar' and return an additional value.

Like 'modify', but doesn't block. Returns 'Just' the additional value if the
contents could be replaced, 'Nothing' otherwise.
-}
tryModify ∷ RWVar α → (α → IO (α, β)) → IO (Maybe β)
tryModify (RWVar l r) = RWLock.tryWithWrite l ∘ modifyIORefM r


-- The End --------------------------------------------------------------------
