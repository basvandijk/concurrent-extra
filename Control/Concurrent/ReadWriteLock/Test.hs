{-# LANGUAGE CPP, NoImplicitPrelude , UnicodeSyntax #-}

module Control.Concurrent.ReadWriteLock.Test ( tests ) where


-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

-- from base:
import Control.Monad      ( (>>), (>>=), forever, replicateM_ )
import Control.Concurrent ( forkIO, threadDelay )
import Data.Function      ( ($) )
import System.IO          ( putStr )
import System.Random      ( randomRIO )
import System.Timeout     ( timeout )

#if __GLASGOW_HASKELL__ < 700
import Prelude            ( fromInteger )
import Control.Monad      ( (>>=), fail )
#endif

-- from base-unicode-symbols:
import Prelude.Unicode    ( (⋅) )

-- from concurrent-extra:
import qualified Control.Concurrent.ReadWriteLock as RWLock
    ( new, acquireWrite, acquireRead, releaseWrite, releaseRead, withRead, withWrite  )

import TestUtils ( within, a_moment )

import Utils ( void )

-- from HUnit:
import Test.HUnit ( Assertion, assert )

-- from test-framework:
import Test.Framework  ( Test )

-- from test-framework-hunit:
import Test.Framework.Providers.HUnit ( testCase )


-------------------------------------------------------------------------------
-- Tests for ReadWriteLock
-------------------------------------------------------------------------------

tests ∷ [Test]
tests = [ testCase "test1" test1
        , testCase "test2" test2
        , testCase "stressTest" stressTest
        ]

test1 ∷ Assertion
test1 = assert $ within (10 ⋅ a_moment) $ do
          -- Create a new read-write-lock (in the "Free" state):
          rwl ← RWLock.new

          -- Put the read-write-lock in the "Write" state:
          RWLock.acquireWrite rwl

          -- Fork a thread that releases the write-lock after a moment:
          void $ forkIO $ threadDelay a_moment >> RWLock.releaseWrite rwl

          -- This blocks until the write-lock is released in the above thread.
          RWLock.acquireRead rwl

          -- Release the read-lock so that the read-write-lock can either be
          -- acquired again by 'acquireRead' or 'acquireWrite':
          RWLock.releaseRead rwl

          -- The read-write-lock should now be in the "Free" state so the
          -- following shouldn't deadlock:
          RWLock.acquireWrite rwl

test2 ∷ Assertion
test2 = assert $ within (10 ⋅ a_moment) $ do
          -- Create a new read-write-lock (in the "Free" state):
          rwl ← RWLock.new

          -- Put the read-write-lock in the "Read" state:
          RWLock.acquireRead rwl

          -- Fork a thread that releases the read-lock after a moment:
          void $ forkIO $ threadDelay a_moment >> RWLock.releaseRead rwl

          -- This blocks until the read-lock is released in the above thread.
          RWLock.acquireWrite rwl

          -- Release the write-lock so that the read-write-lock can either be
          -- acquired again by 'acquireRead' or 'acquireWrite':
          RWLock.releaseWrite rwl

          -- The read-write-lock should now be in the "Free" state so the
          -- following shouldn't deadlock:
          RWLock.acquireRead rwl

stressTest :: Assertion
stressTest = assert $ within (500 ⋅ a_moment) $ do
  lock <- RWLock.new
  
  let
    reader = forever $ do
      randomRIO (0, 100) >>= threadDelay
      RWLock.withRead lock $ putStr "r"
      
    writer = forever $ do
      randomRIO (0, 100) >>= threadDelay
      RWLock.withWrite lock $ putStr "w"
    
  replicateM_ 10 $ forkIO reader
  replicateM_ 10 $ forkIO writer
  timeout (250 ⋅ a_moment) writer
  
