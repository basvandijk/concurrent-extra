{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude #-}

module Control.Concurrent.Lock ( Lock
                               , new
                               , acquire
                               , tryAcquire
                               , release
                               , with
                               , locked
                               ) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Control.Concurrent.MVar ( MVar
                               , newMVar
                               , takeMVar
                               , tryTakeMVar
                               , tryPutMVar
                               , isEmptyMVar
                               )
import Control.Exception       ( bracket_ )
import Control.Monad           ( Monad, return, (>>=), fail
                               , fmap
                               )
import Control.Applicative     ( (<$>), liftA2 )
import System.IO               ( IO )
import Data.Maybe              ( isJust )
import Data.Function           ( flip )
import Data.Bool               ( Bool )

-- from base-unicode-symbols
import Data.Function.Unicode   ( (∘) )


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

newtype Lock = Lock { un ∷ MVar () }

new ∷ IO Lock
new = Lock <$> newMVar ()

acquire ∷ Lock → IO ()
acquire = takeMVar ∘ un

tryAcquire ∷ Lock → IO Bool
tryAcquire = fmap isJust ∘ tryTakeMVar ∘ un

release ∷ Lock → IO ()
release = void ∘ flip tryPutMVar () ∘ un

with ∷ Lock → IO a → IO a
with = liftA2 bracket_ acquire release

locked ∷ Lock → IO Bool
locked = isEmptyMVar ∘ un

void ∷ Monad m ⇒ m a → m ()
void m = do _ ← m
            return ()


-- The End ---------------------------------------------------------------------
