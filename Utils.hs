{-# LANGUAGE NoImplicitPrelude, UnicodeSyntax #-}

module Utils where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Control.Concurrent.MVar ( MVar, takeMVar, putMVar )
import Control.Exception       ( SomeException(SomeException), block, throwIO )
import Control.Monad           ( Monad, return, (>>=), (>>), fail )
import Data.Bool               ( Bool(False, True), otherwise )
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

void ∷ Functor f ⇒ f α → f ()
void = (() <$)

ifM ∷ Monad m ⇒ m Bool → m α → m α → m α
ifM c t e = c >>= \b → if b then t else e

anyM ∷ Monad m ⇒ (α → m Bool) → [α] → m Bool
anyM f = anyM_f
    where
      anyM_f []     = return False
      anyM_f (x:xs) = ifM (f x) (return True) (anyM_f xs)

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

{-| Strictly delete the first element of the list for which the predicate holds.

Note that this function has the following strictness properties:

@deleteWhen' (== 2) (1:2:3:undefined) = 1:3:undefined@
@deleteWhen' (== 3) (1:2:3:undefined) = undefined@
-}
deleteWhen' ∷ (α → Bool) → [α] -> [α]
deleteWhen' p = deleteWhen'_p
    where
      deleteWhen'_p []     = []
      deleteWhen'_p (x:xs)
          | p x            = xs
          | otherwise      = (x:) $! deleteWhen'_p xs


-- The End ---------------------------------------------------------------------
