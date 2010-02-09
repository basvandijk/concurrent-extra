{-# LANGUAGE NoImplicitPrelude, UnicodeSyntax, ScopedTypeVariables #-}

module Main where

-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

-- from base
import Control.Applicative ( (<$>) )
import Control.Concurrent  ( forkIO, threadDelay )
import Control.Exception   ( catch, try, throwTo, ErrorCall(..), SomeException )
import Control.Monad       ( (>>=), (>>), return, fail, fmap
                           , mapM_, replicateM, replicateM_
                           )
import Data.Bool           ( Bool, not )
import Data.Char           ( String )
import Data.Either         ( Either(Left, Right) )
import Data.Function       ( ($) )
import Data.Int            ( Int )
import Data.Maybe          ( isJust )
import Prelude             ( fromInteger )
import System.IO           ( IO )
import System.Timeout      ( timeout )

-- from base-unicode-symbols
import Data.Function.Unicode ( (∘) )
import Prelude.Unicode       ( (⋅) )

-- from concurrent-extra
import qualified Control.Concurrent.Lock  as Lock
import qualified Control.Concurrent.Event as Event

-- from HUnit
import Test.HUnit hiding ( Test )

-- from test-framework
import Test.Framework  ( Test, defaultMain, testGroup )

-- from test-framework-hunit
import Test.Framework.Providers.HUnit ( testCase )



-------------------------------------------------------------------------------
-- Tests
-------------------------------------------------------------------------------

main ∷ IO ()
main = defaultMain tests

tests ∷ [Test]
tests = [ testGroup "Events"
          [ testCase "set wait a"    $ test_event_1 1 1
          , testCase "set wait b"    $ test_event_1 5 1
          , testCase "set wait c"    $ test_event_1 1 5
          , testCase "set wait d"    $ test_event_1 5 5
          , testCase "conc set wait" $ test_event_2
          , testCase "multi wake"    $ test_event_3 10
          , testCase "exception"     $ test_event_4
          , testCase "wait timeout"  $ test_event_5
          , testCase "foo!"          $ test_event_6
          ]
        , testGroup "Lock"
          [ testCase "acquire release"    test_lock_1
          , testCase "acquire acquire"    test_lock_2
          , testCase "new release"        test_lock_3
          , testCase "new unlocked"       test_lock_4
          , testCase "newAcquired locked" test_lock_5
          , testCase "acq rel unlocked"   test_lock_6
          , testCase "conc release"       test_lock_7
          ]
        , testGroup "RLock"
          [
          ]
        ]


--------------------------------------------------------------------------------
-- Events
--------------------------------------------------------------------------------

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


--------------------------------------------------------------------------------
-- Locks
--------------------------------------------------------------------------------

test_lock_1 ∷ Assertion
test_lock_1 = assert $ within a_moment $ do
  l ← Lock.new
  Lock.acquire l
  Lock.release l

test_lock_2 ∷ Assertion
test_lock_2 = assert $ notWithin (10 ⋅ a_moment) $ do
  l ← Lock.new
  Lock.acquire l
  Lock.acquire l

test_lock_3 ∷ Assertion
test_lock_3 = assertException "" $ do
  l ← Lock.new
  Lock.release l

test_lock_4 ∷ Assertion
test_lock_4 = assert $ do
  l ← Lock.new
  fmap not $ Lock.locked l

test_lock_5 ∷ Assertion
test_lock_5 = assert $ do
  l ← Lock.newAcquired
  Lock.locked l

test_lock_6 ∷ Assertion
test_lock_6 = assert $ do
  l ← Lock.new
  Lock.acquire l
  Lock.release l
  fmap not $ Lock.locked l

test_lock_7 ∷ Assertion
test_lock_7 = assert ∘ within (10 ⋅ a_moment) $ do
  l ← Lock.newAcquired
  _ ← forkIO $ wait_a_moment >> Lock.release l
  Lock.acquire l


--------------------------------------------------------------------------------
-- RLocks
--------------------------------------------------------------------------------

-- TODO

-------------------------------------------------------------------------------
-- Misc
-------------------------------------------------------------------------------

-- Exactly 1 moment. Currently equal to 0.005 seconds.
a_moment ∷ Int
a_moment = 5000

wait_a_moment ∷ IO ()
wait_a_moment = threadDelay a_moment

-- True if the action 'a' evaluates within 't' μs.
within ∷ Int → IO α → IO Bool
within t a = isJust <$> timeout t a

notWithin ∷ Int → IO α → IO Bool
notWithin t a = not <$> within t a

assertException ∷ String → IO α → Assertion
assertException errMsg a = do e ← try a
                              case e of
                                Left (_ ∷ SomeException ) → return ()
                                Right _ → assertFailure errMsg
