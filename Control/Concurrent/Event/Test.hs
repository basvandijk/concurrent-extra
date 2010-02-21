{-# LANGUAGE NoImplicitPrelude
           , UnicodeSyntax
           , ScopedTypeVariables  
  #-}

module Control.Concurrent.Event.Test ( tests ) where

-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

-- from base:
import Control.Exception  ( catch, throwTo, ErrorCall(..) )
import Control.Concurrent ( forkIO )
import Control.Monad      ( return, (>>=), fail, (>>)
                          , mapM_, replicateM, replicateM_ 
                          )
import Data.Function      ( ($) )
import Data.Int           ( Int )
import Prelude            ( fromInteger )

-- from base-unicode-symbols:
import Prelude.Unicode ( (⋅) )

-- from concurrent-extra:
import qualified Control.Concurrent.Event as Event
import TestUtils

-- from HUnit:
import Test.HUnit ( Assertion, assert )

-- from test-framework:
import Test.Framework  ( Test )

-- from test-framework-hunit:
import Test.Framework.Providers.HUnit ( testCase )


-------------------------------------------------------------------------------
-- Tests for Event
-------------------------------------------------------------------------------

tests ∷ [Test]
tests = [ testCase "set wait a"    $ test_event_1 1 1
        , testCase "set wait b"    $ test_event_1 5 1
        , testCase "set wait c"    $ test_event_1 1 5
        , testCase "set wait d"    $ test_event_1 5 5
        , testCase "conc set wait" $ test_event_2
        , testCase "multi wake"    $ test_event_3 10
        , testCase "exception"     $ test_event_4
        , testCase "wait timeout"  $ test_event_5
        , testCase "wait blocks"   $ test_event_6
        ]

-- Set an event 's' times then wait for it 'w' times. This should
-- terminate within a few moments.
test_event_1 ∷ Int → Int → Assertion
test_event_1 s w = assert $ within (10 ⋅ a_moment) $ do
  e ← Event.new
  replicateM_ s $ Event.set  e
  replicateM_ w $ Event.wait e

test_event_2 ∷ Assertion
test_event_2 = assert $ within (10 ⋅ a_moment) $ do
  e1 ← Event.new
  e2 ← Event.new
  _ ← forkIO $ do
    Event.wait e1
    Event.set  e2
  wait_a_moment
  Event.set  e1
  Event.wait e2

-- Waking multiple threads with a single Event.
test_event_3 ∷ Int → Assertion
test_event_3 n = assert $ within (10 ⋅ a_moment) $ do
  e1 ← Event.new
  es ← replicateM n $ do
    e2 ← Event.new
    _ ← forkIO $ do
      Event.wait e1
      Event.set  e2
    return e2
  wait_a_moment
  Event.set e1
  mapM_ Event.wait es

-- Exception handling while waiting for an Event.
test_event_4 ∷ Assertion
test_event_4 = assert $ within (10 ⋅ a_moment) $ do
  e1 ← Event.new
  e2 ← Event.new
  helperId ← forkIO $ catch (Event.wait e1) $ \(_ ∷ ErrorCall) → Event.set e2
  wait_a_moment
  throwTo helperId $ ErrorCall "Boo!"
  Event.wait e2

test_event_5 ∷ Assertion
test_event_5 = assert $ within (10 ⋅ a_moment) $ do
  e ← Event.new
  Event.waitTimeout e a_moment

test_event_6 ∷ Assertion
test_event_6 = assert $ notWithin (10 ⋅ a_moment) $ do
  e ← Event.new
  Event.wait e


-- The End ---------------------------------------------------------------------
