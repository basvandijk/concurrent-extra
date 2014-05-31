{-# LANGUAGE CPP, NoImplicitPrelude #-}

module Utils
    ( mask
    , mask_
    , (.!)
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
import Data.Function           ( ($), (.) )
import Data.IORef              ( IORef, readIORef, writeIORef )
import Prelude                 ( ($!) )
import System.IO               ( IO )

#if __GLASGOW_HASKELL__ < 700
import Control.Monad           ( (>>), fail )
#endif


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

mask :: ((IO a -> IO a) -> IO b) -> IO b
mask io = blocked >>= \b -> if b then io id else block $ io unblock

mask_ :: IO a -> IO a
mask_ = block

void :: (Functor f) => f a -> f ()
void = (() <$)
#endif

-- | Strict function composition.
(.!) :: (b -> γ) -> (a -> b) -> (a -> γ)
f .! g = (f $!) . g

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM c t e = c >>= \b -> if b then t else e

purelyModifyMVar :: MVar a -> (a -> a) -> IO ()
purelyModifyMVar mv f = mask_ $ takeMVar mv >>= putMVar mv .! f

modifyIORefM :: IORef a -> (a -> IO (a, b)) -> IO b
modifyIORefM r f = do (y, z) <- readIORef r >>= f
                      writeIORef r y
                      return z

modifyIORefM_ :: IORef a -> (a -> IO a) -> IO ()
modifyIORefM_ r f = readIORef r >>= f >>= writeIORef r
