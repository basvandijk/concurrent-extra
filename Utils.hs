{-# LANGUAGE NoImplicitPrelude, UnicodeSyntax #-}

module Utils where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Control.Concurrent.MVar ( MVar, takeMVar, putMVar )
import Control.Exception       ( block )
import Control.Monad           ( Monad, return, (>>=), (>>), fail )
import Data.Bool               ( Bool )
import Data.Function           ( ($) )
import Data.Functor            ( Functor, (<$) )
import Data.IORef              ( IORef, readIORef, writeIORef )
import Prelude                 ( ($!) )
import System.IO               ( IO )

-- from base-unicode-symbols:
import Data.Function.Unicode   ( (∘) )


--------------------------------------------------------------------------------
-- Utility functions
--------------------------------------------------------------------------------

-- | Strict function composition.
(∘!) ∷ (β → γ) → (α → β) → (α → γ)
f ∘! g = (f $!) ∘ g

void ∷ Functor f ⇒ f α → f ()
void = (() <$)

ifM ∷ Monad m ⇒ m Bool → m α → m α → m α
ifM c t e = c >>= \b → if b then t else e

purelyModifyMVar ∷ MVar α → (α → α) → IO ()
purelyModifyMVar mv f = block $ takeMVar mv >>= putMVar mv ∘! f

modifyIORefM ∷ IORef α → (α → IO (α, β)) → IO β
modifyIORefM r f = do (y, z) ← readIORef r >>= f
                      writeIORef r y
                      return z

modifyIORefM_ ∷ IORef α → (α → IO α) → IO ()
modifyIORefM_ r f = readIORef r >>= f >>= writeIORef r


-- The End ---------------------------------------------------------------------
