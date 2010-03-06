{-# LANGUAGE DeriveDataTypeable, NoImplicitPrelude, UnicodeSyntax #-}

-------------------------------------------------------------------------------
-- |
-- Module     : Control.Concurrent.Broadcast
-- Copyright  : (c) 2010 Bas van Dijk & Roel van Dijk
-- License    : BSD3 (see the file LICENSE)
-- Maintainer : Bas van Dijk <v.dijk.bas@gmail.com>
--            , Roel van Dijk <vandijk.roel@gmail.com>
--
-- A Broadcast variable is a mechanism for communication between
-- threads. Multiple listening threads can wait until a broadcaster thread
-- broadcasts a value. The listeners block until the value is received. When the
-- broadcaster broadcasts the value all listeners are woken.
--
-- All functions are /exception safe/. Throwing asynchronous exceptions will not
-- compromise the internal state of a 'Broadcast' variable.
--
-- This module is designed to be imported qualified. We suggest importing it
-- like:
--
-- @
-- import           Control.Concurrent.Broadcast              ( Broadcast )
-- import qualified Control.Concurrent.Broadcast as Broadcast ( ... )
-- @
-------------------------------------------------------------------------------

module Control.Concurrent.Broadcast
  ( Broadcast

    -- * Creating broadcast variables
  , new
  , newBroadcasting

    -- * Listening to broadcasts
  , listen
  , tryListen
  , listenTimeout

    -- * Broadcasting
  , broadcast
  , signal
  , silence
  ) where


-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

-- from base:
import Control.Applicative     ( (<$>) )
import Control.Monad           ( (>>=), (>>), return, fmap, forM_, fail, when )
import Control.Concurrent.MVar ( MVar, newMVar, newEmptyMVar
                               , takeMVar, putMVar, readMVar, modifyMVar_
                               )
import Control.Exception       ( block, unblock )
import Data.Eq                 ( Eq )
import Data.Either             ( Either(Left ,Right), either )
import Data.Function           ( ($), const )
import Data.List               ( delete, length )
import Data.Maybe              ( Maybe(Nothing, Just), isNothing )
import Data.Ord                ( Ord, max )
import Data.Typeable           ( Typeable )
import Prelude                 ( Integer, fromInteger, seq )
import System.IO               ( IO )

-- from base-unicode-symbols:
import Data.Function.Unicode   ( (∘) )

-- from concurrent-extra:
import Utils                      ( purelyModifyMVar )
import Control.Concurrent.Timeout ( timeout )


-------------------------------------------------------------------------------
-- Broadcast
-------------------------------------------------------------------------------

{-|
A broadcast variable is in one of the two states:

* \"Silent\": @'listen'ing@ to the broadcast will block until a value is
@'broadcast'ed@.

* \"Broadcasting @x@\": @'listen'ing@ to the broadcast will return @x@ without
blocking.
-}
newtype Broadcast α = Broadcast {unBroadcast ∷ MVar (Either [MVar α] α)}
    deriving (Eq, Typeable)

-- | @new@ creates a broadcast variable in the \"silent\" state.
new ∷ IO (Broadcast α)
new = Broadcast <$> newMVar (Left [])

-- | @newBroadcasting x@ creates a broadcast variable in the \"broadcasting
-- @x@\" state.
newBroadcasting ∷ α → IO (Broadcast α)
newBroadcasting x = Broadcast <$> newMVar (Right x)

{-|
Listen to a broadcast.

* If the broadcast variable is \"broadcasting @x@\", @listen@ will return @x@
immediately.

* If the broadcast variable is \"silent\", @listen@ will block until another
thread @'broadcast's@ a value to the broadcast variable.
-}
listen ∷ Broadcast α → IO α
listen (Broadcast mv) = block $ do
  mx ← takeMVar mv
  case mx of
    Left rs → do r ← newEmptyMVar
                 putMVar mv $ Left (r:rs)
                 takeMVar r
    Right x → do putMVar mv mx
                 return x

{-|
Try to listen to a broadcast variable; non blocking.

* If the broadcast variable is \"broadcasting @x@\", @tryListen@ will return
'Just' @x@ immediately.

* If the broadcast variable is \"silent\", @tryListen@ returns 'Nothing'
immediately.
-}
tryListen ∷ Broadcast α → IO (Maybe α)
tryListen = fmap (either (const Nothing) Just) ∘ readMVar ∘ unBroadcast

{-|
Listen to a broadcast if it is available within a given amount of time.

Like 'listen', but with a timeout. A return value of 'Nothing' indicates a
timeout occurred.

The timeout is specified in microseconds.

If the broadcast variable is \"silent\" and a timeout of 0 &#x3bc;s is specified
the function returns 'Nothing' without blocking.

Negative timeouts are treated the same as a timeout of 0 &#x3bc;s.
-}
listenTimeout ∷ Broadcast α → Integer → IO (Maybe α)
listenTimeout (Broadcast mv) time = block $ do
  mx ← takeMVar mv
  case mx of
    Left rs → do r ← newEmptyMVar
                 putMVar mv $ Left (r:rs)
                 my ← unblock $ timeout (max time 0) (takeMVar r)
                 when (isNothing my) $ deleteReader r
                 return my
    Right x  → do putMVar mv mx
                  return $ Just x
    where
      deleteReader r = do mx ← takeMVar mv
                          case mx of
                            Left rs → do let rs' = delete r rs
                                         length rs' `seq` putMVar mv (Left rs')
                            Right _ → putMVar mv mx

{-|
Broadcast a value.

@broadcast b x@ changes the state of the broadcast variable @b@ to
\"broadcasting @x@\".

If the broadcast variable was \"silent\" all threads that are @'listen'ing@ to
the broadcast variable will be woken.
-}
broadcast ∷ Broadcast α → α → IO ()
broadcast (Broadcast mv) x = modifyMVar_ mv $ \mx -> do
                               case mx of
                                 Left rs → do forM_ rs $ \r → putMVar r x
                                              return $ Right x
                                 Right _ → return $ Right x
{-|
Signal a value before becoming \"silent\".

The state of the broadcast variable is changed to \"silent\" after all threads
that are @'listen'ing@ to the broadcast are woken and resume with the signalled
value.
-}
signal ∷ Broadcast α → α → IO ()
signal (Broadcast mv) x = modifyMVar_ mv $ \mx -> do
                            case mx of
                              Left rs → do forM_ rs $ \r → putMVar r x
                                           return $ Left []
                              Right _ → return $ Left []

-- | Set a broadcast variable to the \"silent\" state.
silence ∷ Broadcast α → IO ()
silence (Broadcast mv) = purelyModifyMVar mv $ either Left $ const $ Left []


-- The End ---------------------------------------------------------------------
