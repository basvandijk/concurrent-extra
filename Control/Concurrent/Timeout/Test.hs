{-# LANGUAGE CPP, NoImplicitPrelude, UnicodeSyntax #-}

module Control.Concurrent.Timeout.Test ( tests ) where


-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

-- from base:
import Control.Concurrent ( forkIO, killThread )
import Data.Function      ( ($) )
import Prelude            ( toInteger )

#if __GLASGOW_HASKELL__ < 701
import Prelude            ( fromInteger )
import Control.Monad      ( (>>=), fail, (>>) )
#endif

-- from base-unicode-symbols:
import Prelude.Unicode    ( (⋅) )

-- from concurrent-extra:
import qualified Control.Concurrent.Lock   as Lock
import Control.Concurrent.Timeout ( timeout )
import TestUtils ( a_moment, within )
import Utils ( void, mask_ )

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
  tid ← mask_ $ forkIO $
          void $ timeout (toInteger $ 1000 ⋅ a_moment) $ Lock.acquire l
  killThread tid
  Lock.release l


-- The End ---------------------------------------------------------------------
