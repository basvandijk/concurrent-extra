{-# LANGUAGE CPP
           , DeriveDataTypeable
           , NoImplicitPrelude
           , UnicodeSyntax
  #-}

-------------------------------------------------------------------------------
-- |
-- Module     : Control.Concurrent.Event
-- Copyright  : (c) 2010 Bas van Dijk & Roel van Dijk
-- License    : BSD3 (see the file LICENSE)
-- Maintainer : Bas van Dijk <v.dijk.bas@gmail.com>
--            , Roel van Dijk <vandijk.roel@gmail.com>
--
-- An Event is a simple mechanism for communication between threads: one thread
-- signals an event and other threads wait for it.
--
-- An event has a state which is either \"set\" or \"cleared\". This state can
-- be changed with the corresponding functions 'set' and 'clear'. The 'wait'
-- function blocks until the state is \"set\". An important property of setting
-- an event is that /all/ threads waiting for it are woken.
--
-- It was inspired by the Python @Event@ object. See:
--
-- <http://docs.python.org/3.1/library/threading.html#event-objects>
--
-- This module is designed to be imported qualified. We suggest importing it
-- like:
--
-- @
-- import           Control.Concurrent.Event          ( Event )
-- import qualified Control.Concurrent.Event as Event ( ... )
-- @
--
-------------------------------------------------------------------------------

module Control.Concurrent.Event
  ( Event

    -- * Creating events
  , new
  , newSet

    -- * Waiting for events
  , wait
  , waitTimeout
  , isSet

    -- * Setting events
  , set
  , signal
  , clear
  ) where


-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

-- from base
import Data.Bool               ( Bool(..) )
import Data.Eq                 ( Eq )
import Data.Functor            ( fmap, (<$>) )
import Data.Maybe              ( isJust )
import Data.Typeable           ( Typeable )
#ifdef __HADDOCK__
import Control.Exception       ( block )
#endif
import Prelude                 ( Integer )
import System.IO               ( IO )

-- from base-unicode-symbols
import Data.Function.Unicode   ( (∘) )

-- from concurrent-extra
import           Control.Concurrent.Broadcast ( Broadcast )
import qualified Control.Concurrent.Broadcast as Broadcast
    ( new, newBroadcasting
    , listen, tryListen, listenTimeout
    , broadcast, signal, silence
    )


-------------------------------------------------------------------------------
-- Events
-------------------------------------------------------------------------------

-- | An event is in one of two possible states: \"set\" or \"cleared\".
newtype Event = Event {evBroadcast ∷ Broadcast ()} deriving (Eq, Typeable)


-------------------------------------------------------------------------------
-- Creating events
-------------------------------------------------------------------------------

-- | Create an event in the \"cleared\" state.
new ∷ IO Event
new = Event <$> Broadcast.new

-- | Create an event in the \"set\" state.
newSet ∷ IO Event
newSet = Event <$> Broadcast.newBroadcasting ()


-------------------------------------------------------------------------------
-- Waiting for events
-------------------------------------------------------------------------------

{-|
Block until the event is 'set'.

If the state of the event is already \"set\" this function will return
immediately. Otherwise it will block until another thread calls 'set'.

(You can also resume a thread that is waiting for an event by throwing an
asynchronous exception.)
-}
wait ∷ Event → IO ()
wait = Broadcast.listen ∘ evBroadcast

{-|
Block until the event is 'set' or until a timer expires.

Like 'wait', but with a timeout. A return value of 'False' indicates a timeout
occurred.

The timeout is specified in microseconds.

If the event is \"cleared\" and a timeout of 0 &#x3bc;s is specified the
function returns 'False' without blocking.

Negative timeouts are treated the same as a timeout of 0 &#x3bc;s.
-}
waitTimeout ∷ Event → Integer → IO Bool
waitTimeout ev time = isJust <$> Broadcast.listenTimeout (evBroadcast ev) time

{-|
Returns 'True' if the state of the event is \"set\" and 'False' if the state
is \"cleared\".

Notice that this is only a snapshot of the state. By the time a program reacts
on its result it may already be out of date.
-}
isSet ∷ Event → IO Bool
isSet = fmap isJust ∘ Broadcast.tryListen ∘ evBroadcast


-------------------------------------------------------------------------------
-- Setting events
-------------------------------------------------------------------------------

{-|
Changes the state of the event to \"set\". All threads that where waiting
for this event are woken. Threads that 'wait' after the state is changed to
\"set\" will not block at all.
-}
set ∷ Event → IO ()
set ev = Broadcast.broadcast (evBroadcast ev) ()

{-|
Changes the state to \"cleared\" after all threads that where waiting for this
event are woken. Threads that 'wait' after a @signal@ will block until the event
is 'set' again.

The semantics of signal are equivalent to the following definition:

@
  signal e = 'block' $ 'set' e >> 'clear' e
@-}
signal ∷ Event → IO ()
signal ev = Broadcast.signal (evBroadcast ev) ()

{-|
Changes the state of the event to \"cleared\". Threads that 'wait' after the
state is changed to \"cleared\" will block until the state is changed to \"set\".
-}
clear ∷ Event → IO ()
clear = Broadcast.silence ∘ evBroadcast


-- The End ---------------------------------------------------------------------
