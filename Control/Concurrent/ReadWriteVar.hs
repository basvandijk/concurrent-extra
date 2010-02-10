{-# LANGUAGE NoImplicitPrelude, TupleSections, UnicodeSyntax #-}

module Control.Concurrent.ReadWriteVar
  ( RWVar
  , new
  , with
  -- , tryWith
  , modify
  , modify_
  , tryModify
  , tryModify_
  ) where


-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

-- from base
import Control.Exception       ( bracket_ )
import Control.Monad           ( return, (>>=), return, (=<<), fail, (>>)
                               , when, fmap, liftM2
                               )
import Data.Bool               ( Bool )
import Data.Function           ( ($) )
import Data.Maybe              ( Maybe, isJust )
import Data.IORef              ( IORef, newIORef, readIORef, writeIORef
                               , modifyIORef
                               )
import Prelude                 ( Integer, fromInteger, succ, pred, ($!) )
import System.IO               ( IO )

-- from base-unicode-symbols
import Data.Function.Unicode   ( (∘) )
import Data.Eq.Unicode         ( (≡) )

-- from concurrent-extra
import           Control.Concurrent.ReadWriteLock ( RWLock )
import qualified Control.Concurrent.ReadWriteLock as RWLock


-------------------------------------------------------------------------------
-- Read Write Variables: concurrent read, sequential write
-------------------------------------------------------------------------------

data RWVar α = RWVar RWLock (IORef α)

new ∷ α → IO (RWVar α)
new = liftM2 RWVar RWLock.new ∘ newIORef

with ∷ RWVar α → (α → IO β) → IO β
with (RWVar l r) f = RWLock.withRead l $ readIORef r >>= f

--tryWith ∷ RWVar α → (α → IO β) → IO (Maybe β)
--tryWith (RWVar l r) f = RWLock.tryWithRead l $ readIORef r >>= f

modify ∷ RWVar α → (α → IO (α, β)) → IO β
modify (RWVar l r) = RWLock.withWrite l ∘ modifyIORefM r

modify_ ∷ RWVar α → (α → IO α) → IO ()
modify_ (RWVar l r) = RWLock.withWrite l ∘ modifyIORefM_ r

tryModify ∷ RWVar α → (α → IO (α, β)) → IO (Maybe β)
tryModify (RWVar l r) = RWLock.tryWithWrite l ∘ modifyIORefM r

tryModify_ ∷ RWVar α → (α → IO α) → IO Bool
tryModify_ (RWVar l r) = fmap isJust ∘ RWLock.tryWithWrite l ∘ modifyIORefM_ r


-------------------------------------------------------------------------------

modifyIORefM ∷ IORef α → (α → IO (α, β)) → IO β
modifyIORefM r f = do x ← readIORef r
                      (y, z) ← f x
                      writeIORef r y
                      return z

modifyIORefM_ ∷ IORef α → (α → IO α) → IO ()
modifyIORefM_ r f = do x ← readIORef r
                       y ← f x
                       writeIORef r y


-- The End --------------------------------------------------------------------


