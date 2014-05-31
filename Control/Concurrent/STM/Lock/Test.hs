{-# LANGUAGE CPP
           , NoImplicitPrelude
           , ScopedTypeVariables
  #-}

module Control.Concurrent.STM.Lock.Test ( tests ) where

-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

-- from base:
import Prelude            ( (*) )
import Control.Concurrent ( forkIO )
import Control.Monad      ( return, (>>=), (>>) )
import Data.Bool          ( Bool(False, True), not, (&&) )
import Data.Function      ( ($), (.) )
import Data.Functor       ( fmap )
import Data.IORef         ( newIORef, writeIORef, readIORef )

#if __GLASGOW_HASKELL__ < 700
import Prelude            ( fromInteger )
import Control.Monad      ( fail )
#endif

-- from stm:
import Control.Concurrent.STM ( atomically )

-- from concurrent-extra:
import qualified Control.Concurrent.STM.Lock as Lock
import TestUtils

-- from HUnit:
import Test.HUnit ( Assertion, assert )

-- from test-framework:
import Test.Framework  ( Test )

-- from test-framework-hunit:
import Test.Framework.Providers.HUnit ( testCase )


-------------------------------------------------------------------------------
-- Tests for Lock
-------------------------------------------------------------------------------

tests :: [Test]
tests = [ testCase "acquire release"    test_lock_1
        , testCase "acquire acquire"    test_lock_2
        , testCase "new release"        test_lock_3
        , testCase "new unlocked"       test_lock_4
        , testCase "newAcquired locked" test_lock_5
        , testCase "acq rel unlocked"   test_lock_6
        , testCase "conc release"       test_lock_7
        , testCase "wait"               test_lock_8
        ]

test_lock_1 :: Assertion
test_lock_1 = assert $ within a_moment $ atomically $ do
  l <- Lock.new
  Lock.acquire l
  Lock.release l

test_lock_2 :: Assertion
test_lock_2 = assert $ notWithin (10 * a_moment) $ atomically $ do
  l <- Lock.new
  Lock.acquire l
  Lock.acquire l

test_lock_3 :: Assertion
test_lock_3 = assertException "" $ atomically $ Lock.new >>= Lock.release

test_lock_4 :: Assertion
test_lock_4 = assert $ atomically $ Lock.new >>= fmap not . Lock.locked

test_lock_5 :: Assertion
test_lock_5 = assert $ atomically $ Lock.newAcquired >>= Lock.locked

test_lock_6 :: Assertion
test_lock_6 = assert $ atomically $ do
  l <- Lock.new
  Lock.acquire l
  Lock.release l
  fmap not $ Lock.locked l

test_lock_7 :: Assertion
test_lock_7 = assert . within (10 * a_moment) $ do
  l <- atomically $ Lock.newAcquired
  _ <- forkIO $ wait_a_moment >> atomically (Lock.release l)
  atomically $ Lock.acquire l

test_lock_8 :: Assertion
test_lock_8 = assert $ do
  ioRef <- newIORef False
  l <- atomically Lock.newAcquired
  _ <- forkIO $ do wait_a_moment
                   writeIORef ioRef True
                   atomically $ Lock.release l
  atomically $ Lock.wait l
  set <- readIORef ioRef
  locked <- atomically $ Lock.locked l
  return $ set && not locked
