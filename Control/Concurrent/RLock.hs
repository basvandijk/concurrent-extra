{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude #-}

module Control.Concurrent.RLock
    ( RLock
    , new
    , acquire
    , tryAcquire
    , release
    , with
    , tryWith
    , recursionLevel
    ) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Control.Applicative     ( (<$>), liftA2 )
import Control.Concurrent      ( ThreadId, myThreadId )
import Control.Concurrent.MVar ( MVar, newMVar, takeMVar, readMVar, putMVar )
import Control.Exception       ( block, unblock, bracket_, finally )
import Control.Monad           ( Monad, return, (>>=), fail, (>>), fmap )
import Data.Bool               ( Bool(False, True), otherwise )
import Data.Function           ( ($) )
import Data.Maybe              ( Maybe(Nothing, Just), maybe )
import Data.List               ( (++) )
import Prelude                 ( Integer, fromInteger, succ, pred, error, seq )
import System.IO               ( IO )

-- from base-unicode-symbols
import Data.Eq.Unicode         ( (≡) )
import Data.Function.Unicode   ( (∘) )

-- from ourselves:
import           Control.Concurrent.Lock ( Lock )
import qualified Control.Concurrent.Lock as Lock
    ( newAcquired, acquire, release )


--------------------------------------------------------------------------------
-- Reentrant locks
--------------------------------------------------------------------------------

newtype RLock = RLock {un ∷ MVar (Maybe (ThreadId, Integer, Lock))}

new ∷ IO RLock
new = RLock <$> newMVar Nothing

acquire ∷ RLock → IO ()
acquire (RLock mv) = do
  myTID ← myThreadId
  block $ do
    mb ← takeMVar mv
    case mb of
      Nothing         → do lock ← Lock.newAcquired
                           putMVar mv $ Just (myTID, 1, lock)
      Just (tid, n, lock)
        | myTID ≡ tid → do let sn = succ n
                           sn `seq` putMVar mv $ Just (tid, sn, lock)

        | otherwise   → do putMVar mv mb
                           unblock $ Lock.acquire lock

tryAcquire ∷ RLock → IO Bool
tryAcquire (RLock mv) = do
  myTID ← myThreadId
  block $ do
    mb ← takeMVar mv
    case mb of
      Nothing         → do lock ← Lock.newAcquired
                           putMVar mv $ Just (myTID, 1, lock)
                           return True
      Just (tid, n, lock)
        | myTID ≡ tid → do let sn = succ n
                           sn `seq` putMVar mv $ Just (tid, sn, lock)
                           return True

        | otherwise   → do putMVar mv mb
                           return False

release ∷ RLock → IO ()
release (RLock mv) = do
  myTID ← myThreadId
  block $ do
    mb ← takeMVar mv
    let myError str = do putMVar mv mb
                         error $ "Control.Concurrent.RLock.release: " ++ str
    case mb of
      Nothing → myError "Can't release an unacquired RLock!"
      Just (tid, n, lock)
        | myTID ≡ tid → if n ≡ 1
                        then do Lock.release lock
                                putMVar mv Nothing
                        else do let pn = pred n
                                pn `seq` putMVar mv $ Just (tid, pn, lock)
        | otherwise → myError "Calling thread does not own the RLock!"

with ∷ RLock → IO α → IO α
with = liftA2 bracket_ acquire release

tryWith ∷ RLock → IO α → IO (Maybe α)
tryWith l a = block $ do
  acquired ← tryAcquire l
  if acquired
    then fmap Just $ a `finally` release l
    else return Nothing

recursionLevel ∷ RLock → IO Integer
recursionLevel = fmap (maybe 0 (\(_, n, _) → n)) ∘ readMVar ∘ un


-- The End ---------------------------------------------------------------------
