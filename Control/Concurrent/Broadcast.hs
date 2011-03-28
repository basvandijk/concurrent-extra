{-# LANGUAGE CPP, DeriveDataTypeable, NoImplicitPrelude, UnicodeSyntax #-}

-------------------------------------------------------------------------------
-- |
-- Module     : Control.Concurrent.Broadcast
-- Copyright  : (c) 2010 Bas van Dijk & Roel van Dijk
-- License    : BSD3 (see the file LICENSE)
-- Maintainer : Bas van Dijk <v.dijk.bas@gmail.com>
--            , Roel van Dijk <vandijk.roel@gmail.com>
--
-- A 'Broadcast' is a mechanism for communication between threads. Multiple
-- @'listen'ers@ wait until a broadcaster @'broadcast's@ a value. The listeners
-- block until the value is received. When the broadcaster broadcasts a value
-- all listeners are woken.
--
-- All functions are /exception safe/. Throwing asynchronous exceptions will not
-- compromise the internal state of a broadcast.
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

    -- * Creating broadcasts
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
import Control.Monad              ( return, when )
import Control.Concurrent.MVar    ( MVar, newMVar, newEmptyMVar
                                  , takeMVar, putMVar, readMVar, modifyMVar_
                                  )
import Control.Exception          ( onException )
import Data.Eq                    ( Eq )
import Data.Either                ( Either(Left ,Right), either )
import Data.Function              ( ($), const )
import Data.Functor               ( fmap, (<$>) )
import Data.Foldable              ( for_ )
import Data.List                  ( delete, length )
import Data.Maybe                 ( Maybe(Nothing, Just), isNothing )
import Data.Ord                   ( Ord, max )
import Data.Typeable              ( Typeable )
import Prelude                    ( Integer, seq )
import System.IO                  ( IO )

#if __GLASGOW_HASKELL__ < 700
import Prelude                    ( fromInteger )
import Control.Monad              ( (>>=), (>>), fail )
#endif

-- from base-unicode-symbols:
import Data.Function.Unicode      ( (∘) )

-- from concurrent-extra (this package):
import Utils                      ( purelyModifyMVar, mask_ )
import Control.Concurrent.Timeout ( timeout )


-------------------------------------------------------------------------------
-- Broadcast
-------------------------------------------------------------------------------

{-|
A broadcast is in one of two possible states:

* \"Silent\": @'listen'ing@ to the broadcast will block until a value is
@'broadcast'ed@.

* \"Broadcasting @x@\": @'listen'ing@ to the broadcast will return the value @x@
without blocking.
-}
newtype Broadcast α = Broadcast {unBroadcast ∷ MVar (Either [MVar α] α)}
    deriving (Eq, Typeable)

-- | @new@ creates a broadcast in the \"silent\" state.
new ∷ IO (Broadcast α)
new = Broadcast <$> newMVar (Left [])

-- | @newBroadcasting x@ creates a broadcast in the \"broadcasting @x@\" state.
newBroadcasting ∷ α → IO (Broadcast α)
newBroadcasting x = Broadcast <$> newMVar (Right x)

{-|
Listen to a broadcast.

* If the broadcast is \"broadcasting @x@\", @listen@ will return @x@
immediately.

* If the broadcast is \"silent\", @listen@ will block until another thread
@'broadcast's@ a value to the broadcast.
-}
listen ∷ Broadcast α → IO α
listen (Broadcast mv) = mask_ $ do
  mx ← takeMVar mv
  case mx of
    Left ls → do l ← newEmptyMVar
                 putMVar mv $ Left $ l:ls
                 takeMVar l
    Right x → do putMVar mv mx
                 return x

{-|
Try to listen to a broadcast; non blocking.

* If the broadcast is \"broadcasting @x@\", @tryListen@ will return 'Just' @x@
immediately.

* If the broadcast is \"silent\", @tryListen@ returns 'Nothing' immediately.
-}
tryListen ∷ Broadcast α → IO (Maybe α)
tryListen = fmap (either (const Nothing) Just) ∘ readMVar ∘ unBroadcast

{-|
Listen to a broadcast if it is available within a given amount of time.

Like 'listen', but with a timeout. A return value of 'Nothing' indicates a
timeout occurred.

The timeout is specified in microseconds.

If the broadcast is \"silent\" and a timeout of 0 &#x3bc;s is specified the
function returns 'Nothing' without blocking.

Negative timeouts are treated the same as a timeout of 0 &#x3bc;s.
-}
listenTimeout ∷ Broadcast α → Integer → IO (Maybe α)
listenTimeout (Broadcast mv) time = mask_ $ do
  mx ← takeMVar mv
  case mx of
    Left ls → do l ← newEmptyMVar
                 putMVar mv $ Left $ l:ls
                 my ← timeout (max time 0) (takeMVar l)
                      `onException` deleteReader l
                 when (isNothing my) (deleteReader l)
                 return my
    Right x → do putMVar mv mx
                 return $ Just x
    where
      deleteReader l = do mx ← takeMVar mv
                          case mx of
                            Left ls → let ls' = delete l ls
                                      in length ls' `seq` putMVar mv (Left ls')
                            Right _ → putMVar mv mx

{-|
Broadcast a value.

@broadcast b x@ changes the state of the broadcast @b@ to \"broadcasting @x@\".

If the broadcast was \"silent\" all threads that are @'listen'ing@ to the
broadcast will be woken.
-}
broadcast ∷ Broadcast α → α → IO ()

{-|
Broadcast a value before becoming \"silent\".

The state of the broadcast is changed to \"silent\" after all threads that are
@'listen'ing@ to the broadcast are woken and resume with the signalled value.

The semantics of signal are equivalent to the following definition:

@
  signal b x = 'block' $ 'broadcast' b x >> 'silence' b
@
-}
signal ∷ Broadcast α → α → IO ()

broadcast b x = broadcastThen (Right x) b x
signal    b x = broadcastThen (Left []) b x

-- | Internally used function that performs the actual broadcast in 'broadcast'
-- and 'signal' then changes to the given final state.
broadcastThen ∷ Either [MVar α] α → Broadcast α → α → IO ()
broadcastThen finalState (Broadcast mv) x =
    modifyMVar_ mv $ \mx → do
      case mx of
        Left ls → do for_ ls (`putMVar` x)
                     return finalState
        Right _ → return finalState

-- | Set a broadcast to the \"silent\" state.
silence ∷ Broadcast α → IO ()
silence (Broadcast mv) = purelyModifyMVar mv $ either Left $ const $ Left []


-- The End ---------------------------------------------------------------------
