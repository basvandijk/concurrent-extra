{-# LANGUAGE NamedFieldPuns
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
    -- *Read access
  , acquireRead
  , releaseRead
  , tryAcquireRead
  , withRead
  , tryWithRead
    -- *Write access
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
import Control.Concurrent.MVar ( MVar, newMVar, takeMVar, putMVar )
import Control.Exception       ( block, bracket_, finally )
import Control.Monad           ( return, (>>=), return, fail, (>>)
                               , liftM3, fmap
                               )
import Data.Bool               ( Bool(False, True) )
import Data.Char               ( String )
import Data.Eq                 ( (==) )
import Data.Function           ( ($) )
import Data.Maybe              ( Maybe(Nothing, Just) )
import Prelude                 ( Integer, fromInteger, succ, pred
                               , ($!), error
                               )
import System.IO               ( IO )

-- from base-unicode-symbols
import Data.Monoid.Unicode     ( (⊕) )

-- from concurrent-extra
import           Control.Concurrent.Lock ( Lock )
import qualified Control.Concurrent.Lock as Lock


-------------------------------------------------------------------------------
-- Read Write Lock
-------------------------------------------------------------------------------

data State = Free | Read Integer | Write

data RWLock = RWLock { state     ∷ MVar State
                     , readLock  ∷ Lock
                     , writeLock ∷ Lock
                     }


new ∷ IO RWLock
new = liftM3 RWLock (newMVar Free) Lock.new Lock.new

acquireRead ∷ RWLock → IO ()
acquireRead (RWLock {state, readLock, writeLock}) = do
  st ← takeMVar state
  case st of
    Free   → do Lock.acquire readLock
                putMVar state (Read 1)
    Read n → putMVar state (Read $ succ $! n)
    Write  → do putMVar state st
                Lock.acquire writeLock
                _ ← takeMVar state
                Lock.acquire readLock
                putMVar state (Read 1)

releaseRead ∷ RWLock → IO ()
releaseRead (RWLock {state, readLock}) = do
  st ← takeMVar state
  case st of
    Free   → putMVar state st >> err
    Read 1 → do Lock.release readLock
                putMVar state Free
    Read n → putMVar state (Read $ pred $! n)
    Write  → putMVar state st >> err
  where
    err = error $ moduleName ⊕ ".releaseRead: already released"


tryAcquireRead ∷ RWLock → IO Bool
tryAcquireRead (RWLock {state, readLock}) = do
  st ← takeMVar state
  case st of
    Free   → do Lock.acquire readLock
                putMVar state (Read 1)
                return True
    Read n → do putMVar state (Read $ succ $! n)
                return True
    Write  → do putMVar state st
                return False

withRead ∷ RWLock → IO α → IO α
withRead = liftA2 bracket_ acquireRead releaseRead

tryWithRead ∷ RWLock → IO α → IO (Maybe α)
tryWithRead l a = block $ do
  acquired ← tryAcquireRead l
  if acquired
    then fmap Just $ a `finally` releaseRead l
    else return Nothing

acquireWrite ∷ RWLock → IO ()
acquireWrite (RWLock {state, readLock, writeLock}) = do
  st ← takeMVar state
  case st of
    Free   → do Lock.acquire writeLock
                putMVar state Write
    Read _ → do putMVar state st
                Lock.acquire readLock
                _ ← takeMVar state
                Lock.acquire writeLock
                putMVar state Write
    Write  → do putMVar state st
                error $ moduleName ⊕ ".acquireWrite: already acquired"

tryAcquireWrite ∷ RWLock → IO Bool
tryAcquireWrite (RWLock {state, writeLock}) = do
  st ← takeMVar state
  case st of
    Free   → do Lock.acquire writeLock
                putMVar state Write
                return True
    Read _ → do putMVar state st
                return False
    Write  → do putMVar state st
                error $ moduleName ⊕ ".acquireWrite: already acquired"

releaseWrite ∷ RWLock → IO ()
releaseWrite (RWLock {state, writeLock}) = do
  st ← takeMVar state
  case st of
    Free   → putMVar state st >> err
    Read _ → putMVar state st >> err
    Write  → do Lock.release writeLock
                putMVar state Free
  where
    err = error $ moduleName ⊕ ".releaseWrite: already released"

withWrite ∷ RWLock → IO α → IO α
withWrite = liftA2 bracket_ acquireWrite releaseWrite

tryWithWrite ∷ RWLock → IO α → IO (Maybe α)
tryWithWrite l a = block $ do
  acquired ← tryAcquireWrite l
  if acquired
    then fmap Just $ a `finally` releaseWrite l
    else return Nothing


-------------------------------------------------------------------------------

moduleName ∷ String
moduleName = "Control.Concurrent.ReadWriteLock"


-- The End --------------------------------------------------------------------
