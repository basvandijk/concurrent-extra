{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude #-}

module Control.Concurrent.Lock
    ( Lock
    , new
    , newAcquired
    , acquire
    , tryAcquire
    , release
    , with
    , tryWith
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
import Data.Function           ( ($) )
import Data.Maybe              ( Maybe(Nothing, Just), isJust )
import Prelude                 ( error )
import System.IO               ( IO )

-- from base-unicode-symbols
import Data.Function.Unicode   ( (∘) )


--------------------------------------------------------------------------------
-- Locks
--------------------------------------------------------------------------------

newtype Lock = Lock { un ∷ MVar () }

new ∷ IO Lock
new = Lock <$> newMVar ()

newAcquired ∷ IO Lock
newAcquired = Lock <$> newEmptyMVar

acquire ∷ Lock → IO ()
acquire = takeMVar ∘ un

tryAcquire ∷ Lock → IO Bool
tryAcquire = fmap isJust ∘ tryTakeMVar ∘ un

release ∷ Lock → IO ()
release (Lock mv) = do
  b ← tryPutMVar mv ()
  when (not b) $ error "Control.Concurrent.Lock.release: Can't release unlocked Lock!"

with ∷ Lock → IO a → IO a
with = liftA2 bracket_ acquire release

tryWith ∷ Lock → IO α → IO (Maybe α)
tryWith l a = block $ do
  acquired ← tryAcquire l
  if acquired
    then fmap Just $ a `finally` release l
    else return Nothing

locked ∷ Lock → IO Bool
locked = isEmptyMVar ∘ un


-- The End ---------------------------------------------------------------------
