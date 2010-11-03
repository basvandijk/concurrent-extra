{-# LANGUAGE CPP, NoImplicitPrelude, UnicodeSyntax #-}

-------------------------------------------------------------------------------
-- |
-- Module     : Control.Concurrent.Thread.Delay
-- Copyright  : (c) 2010 Bas van Dijk & Roel van Dijk
-- License    : BSD3 (see the file LICENSE)
-- Maintainer : Bas van Dijk <v.dijk.bas@gmail.com>
--            , Roel van Dijk <vandijk.roel@gmail.com>
--
-- Arbitrarily long thread delays.
-------------------------------------------------------------------------------

module Control.Concurrent.Thread.Delay ( delay ) where


-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

-- from base:
import Control.Concurrent ( threadDelay )
import Control.Monad      ( when )
import Data.Function      ( ($) )
import Data.Int           ( Int )
import Data.Ord           ( min )
import Prelude            ( Integer, toInteger, fromInteger, maxBound, (-) )
import System.IO          ( IO )

#if __GLASGOW_HASKELL__ < 701
import Control.Monad      ( (>>) )
#endif

-- from base-unicode-symbols:
import Data.Eq.Unicode    ( (≢) )


-------------------------------------------------------------------------------
-- Delay
-------------------------------------------------------------------------------

{-|
Like 'threadDelay', but not bounded by an 'Int'.

Suspends the current thread for a given number of microseconds (GHC only).

There is no guarantee that the thread will be rescheduled promptly when the
delay has expired, but the thread will never continue to run earlier than
specified.
-}
delay ∷ Integer → IO ()
delay time = do
  let maxWait = min time $ toInteger (maxBound ∷ Int)
  threadDelay $ fromInteger maxWait
  when (maxWait ≢ time) $ delay (time - maxWait)


-- The End ---------------------------------------------------------------------
