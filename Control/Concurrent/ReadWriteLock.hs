{-# LANGUAGE NamedFieldPuns
           , NoImplicitPrelude
           , TupleSections
           , UnicodeSyntax
  #-}

module Control.Concurrent.ReadWriteLock
  ( RWLock
  , new
  , acquireRead
  , releaseRead
  -- , tryAcquireRead
  , withRead
  -- , tryWithRead
  , acquireWrite
  , releaseWrite
  , tryAcquireWrite
  , withWrite
  , tryWithWrite
  ) where


-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

-- from base
import Control.Applicative     ( liftA2 )
import Control.Exception       ( block, bracket_, finally )
import Control.Monad           ( return, (>>=), return, fail, (>>)
                               , when, liftM3, fmap
                               )
import Data.Bool               ( Bool )
import Data.Function           ( ($) )
import Data.IORef              ( IORef, newIORef, atomicModifyIORef )
import Data.Maybe              ( Maybe(Nothing, Just) )
import Prelude                 ( Integer, fromInteger, succ, pred, ($!) )
import System.IO               ( IO )

-- from base-unicode-symbols
import Data.Function.Unicode   ( (∘) )
import Data.Eq.Unicode         ( (≡) )

-- from concurrent-extra
import           Control.Concurrent.Event ( Event )
import qualified Control.Concurrent.Event as Event
import           Control.Concurrent.Lock ( Lock )
import qualified Control.Concurrent.Lock as Lock


-------------------------------------------------------------------------------
-- Read Write Lock
-------------------------------------------------------------------------------

data RWLock = RWLock { writeLock ∷ Lock
                     , readMode  ∷ Event
                     , readCount ∷ IORef Integer
                     }

new ∷ IO RWLock
new = liftM3 RWLock Lock.new Event.new (newIORef 0)

acquireRead ∷ RWLock → IO ()
acquireRead (RWLock {writeLock, readMode, readCount}) = do
  rc ← swapWith' readCount succ
  if rc ≡ 0
    then Lock.acquire writeLock >> Event.set readMode
    else Event.wait readMode

releaseRead ∷ RWLock → IO ()
releaseRead (RWLock {writeLock, readMode, readCount}) = do
  rc ← swapWith' readCount pred
  when (rc ≡ 1) $ do
    Lock.release writeLock
    Event.clear readMode

{-
tryAcquireRead ∷ RWLock → IO Bool
tryAcquireRead (RWLock {writeLock, readMode, readCount}) = do
  ?
-}

withRead ∷ RWLock → IO α → IO α
withRead = liftA2 bracket_ acquireRead releaseRead

{-
tryWithRead ∷ RWLock → IO α → IO (Maybe α)
tryWithRead l a = block $ do
  acquired ← tryAcquireRead l
  if acquired
    then fmap Just $ a `finally` releaseRead l
    else return Nothing
-}

acquireWrite ∷ RWLock → IO ()
acquireWrite = Lock.acquire ∘ writeLock

tryAcquireWrite ∷ RWLock → IO Bool
tryAcquireWrite = Lock.tryAcquire ∘ writeLock

releaseWrite ∷ RWLock → IO ()
releaseWrite = Lock.release ∘ writeLock

withWrite ∷ RWLock → IO α → IO α
withWrite = liftA2 bracket_ acquireWrite releaseWrite

tryWithWrite ∷ RWLock → IO α → IO (Maybe α)
tryWithWrite l a = block $ do
  acquired ← tryAcquireWrite l
  if acquired
    then fmap Just $ a `finally` releaseWrite l
    else return Nothing

-------------------------------------------------------------------------------

swapWith' ∷ IORef α → (α → α) → IO α
swapWith' r f = atomicModifyIORef r $ \x → (f $! x, x)


-- The End --------------------------------------------------------------------


