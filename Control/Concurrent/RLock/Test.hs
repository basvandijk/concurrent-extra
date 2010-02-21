{-# LANGUAGE NoImplicitPrelude
           , UnicodeSyntax
           , ScopedTypeVariables  
  #-}

module Control.Concurrent.RLock.Test ( tests ) where


-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

-- from base:
import Control.Concurrent ( forkIO, threadDelay )
import Control.Monad      ( (>>=), fail, (>>), replicateM_ )
import Data.Function      ( ($) )
import Data.Int           ( Int )
import Prelude            ( fromInteger )

-- from base-unicode-symbols:
import Data.Function.Unicode ( (∘) )
import Prelude.Unicode       ( (⋅) )

-- from concurrent-extra:
import qualified Control.Concurrent.Event as Event ( new, set, wait )
import qualified Control.Concurrent.RLock as RLock
import TestUtils

-- from HUnit:
import Test.HUnit ( Assertion, assert )

-- from test-framework:
import Test.Framework  ( Test )

-- from test-framework-hunit:
import Test.Framework.Providers.HUnit ( testCase )


-------------------------------------------------------------------------------
-- Tests for RLock
-------------------------------------------------------------------------------

tests ∷ [Test]
tests = [ testCase "recursive acquire"  $ test_rlock_1 5
        , testCase "conc acquire"       $ test_rlock_2
        ]

test_rlock_1 ∷ Int → Assertion
test_rlock_1 n = assert ∘ within (10 ⋅ a_moment) $ do
  l ← RLock.new
  replicateM_ n $ RLock.acquire l
  replicateM_ n $ RLock.release l

-- Tests for bug found by Felipe Lessa.
test_rlock_2 ∷ Assertion
test_rlock_2 = assert ∘ within (20 ⋅ a_moment) $ do
  rl           ← RLock.new
  t1_has_rlock ← Event.new
  t1_done      ← Event.new
  t2_done      ← Event.new

  -- Thread 1
  _ ← forkIO $ do
    RLock.acquire rl
    Event.set t1_has_rlock
    threadDelay $ 10 ⋅ a_moment
    RLock.release rl
    Event.set t1_done

  -- Thread 2
  _ ← forkIO $ do
    Event.wait t1_has_rlock
    RLock.acquire rl
    RLock.release rl
    Event.set t2_done

  Event.wait t1_done
  Event.wait t2_done


-- The End ---------------------------------------------------------------------
