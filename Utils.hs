{-# LANGUAGE CPP, NoImplicitPrelude, UnicodeSyntax #-}

module Utils
    ( mask
    , mask_
    , (∘!)
    , void
    , ifM
    , purelyModifyMVar
    , modifyIORefM
    , modifyIORefM_
    ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Control.Concurrent.MVar ( MVar, takeMVar, putMVar )
import Control.Monad           ( Monad, return, (>>=) )
import Data.Bool               ( Bool )
import Data.Function           ( ($) )
import Data.IORef              ( IORef, readIORef, writeIORef )
import Prelude                 ( ($!) )
import System.IO               ( IO )

#if __GLASGOW_HASKELL__ < 700
import Control.Monad           ( (>>), fail )
#endif

-- from base-unicode-symbols:
import Data.Function.Unicode   ( (∘) )


--------------------------------------------------------------------------------
-- Utility functions
--------------------------------------------------------------------------------

#if MIN_VERSION_base(4,3,0)
import Control.Exception       ( mask, mask_ )
import Control.Monad           ( void )
#else
import Control.Exception       ( blocked, block, unblock )
import Data.Function           ( id )
import Data.Functor            ( Functor, (<$) )

mask ∷ ((IO α → IO α) → IO β) → IO β
mask io = blocked >>= \b → if b then io id else block $ io unblock

mask_ ∷ IO α → IO α
mask_ = block

void ∷ Functor f ⇒ f α → f ()
void = (() <$)
#endif

-- | Strict function composition.
(∘!) ∷ (β → γ) → (α → β) → (α → γ)
f ∘! g = (f $!) ∘ g

ifM ∷ Monad m ⇒ m Bool → m α → m α → m α
ifM c t e = c >>= \b → if b then t else e

purelyModifyMVar ∷ MVar α → (α → α) → IO ()
purelyModifyMVar mv f = mask_ $ takeMVar mv >>= putMVar mv ∘! f

modifyIORefM ∷ IORef α → (α → IO (α, β)) → IO β
modifyIORefM r f = do (y, z) ← readIORef r >>= f
                      writeIORef r y
                      return z

modifyIORefM_ ∷ IORef α → (α → IO α) → IO ()
modifyIORefM_ r f = readIORef r >>= f >>= writeIORef r


-- The End ---------------------------------------------------------------------
