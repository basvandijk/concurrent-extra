{-# LANGUAGE NoImplicitPrelude, UnicodeSyntax #-}

module Utils where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Control.Concurrent.MVar ( MVar, takeMVar, putMVar )
import Control.Exception       ( SomeException(SomeException), block, throwIO )
import Control.Monad           ( Monad, return, (>>=), (>>), fail
                               , Functor, fmap
                               )
import Data.Bool               ( Bool )
import Data.Function           ( ($), const )
import Data.IORef              ( IORef, readIORef, writeIORef )
import System.IO               ( IO )

-- from base-unicode-symbols:
import Data.Function.Unicode   ( (∘) )


--------------------------------------------------------------------------------
-- Utility functions
--------------------------------------------------------------------------------

void ∷ Functor f ⇒ f α → f ()
void = fmap $ const ()

ifM ∷ Monad m ⇒ m Bool → m α → m α → m α
ifM c t e = c >>= \b → if b then t else e

throwInner ∷ SomeException → IO α
throwInner (SomeException e) = throwIO e

purelyModifyMVar ∷ MVar α → (α → α) → IO ()
purelyModifyMVar mv f = block $ takeMVar mv >>= putMVar mv ∘ f

modifyIORefM ∷ IORef α → (α → IO (α, β)) → IO β
modifyIORefM r f = do x ← readIORef r
                      (y, z) ← f x
                      writeIORef r y
                      return z

modifyIORefM_ ∷ IORef α → (α → IO α) → IO ()
modifyIORefM_ r f = readIORef r >>= f >>= writeIORef r


-- The End ---------------------------------------------------------------------
