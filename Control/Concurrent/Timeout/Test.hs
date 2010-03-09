{-# LANGUAGE NoImplicitPrelude
           , UnicodeSyntax
  #-}

module Control.Concurrent.Timeout.Test ( tests ) where


-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

-- from base:
import Control.Concurrent ( )

-- from base-unicode-symbols:
import Prelude.Unicode       ( (⋅) )

-- from concurrent-extra:
import qualified Control.Concurrent.Lock   as Lock
import qualified Control.Concurrent.Thread as Thread
import Control.Concurrent.Timeout ( timeout )
import TestUtils ( a_moment, within )

-- from HUnit:
import Test.HUnit ( Assertion, assert )

-- from test-framework:
import Test.Framework  ( Test )

-- from test-framework-hunit:
import Test.Framework.Providers.HUnit ( testCase )


-------------------------------------------------------------------------------
-- Tests for timeout
-------------------------------------------------------------------------------

tests ∷ [Test]
tests = [ testCase "timeout exception" test_timeout_exception ]

test_timeout_exception ∷ Assertion
test_timeout_exception = assert $ within (10 ⋅ a_moment) $ do
  l ← Lock.newAcquired
  tid ← block $ Thread.forkIO
                $ timeout (toInteger $ 1000 ⋅ a_moment)
                  $ Lock.acquire l
  Thread.killThread tid
  Lock.release l


-- The End ---------------------------------------------------------------------
