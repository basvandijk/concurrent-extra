{-# LANGUAGE DeriveDataTypeable, NoImplicitPrelude, UnicodeSyntax #-}

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
-- Each event has an internal state which is either \"Set\" or \"Cleared\". This
-- state can be changed with the corresponding functions 'set' and 'clear'. The
-- 'wait' function blocks until the state is \"Set\". An important property of
-- setting an event is that /all/ threads waiting for it are woken.
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
  , new
  , newSet
  , wait
  , waitTimeout
  , set
  , clear
  , isSet
  ) where


-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

-- from base
import Control.Applicative     ( (<$>) )
import Control.Monad           ( fmap  )
import Data.Bool               ( Bool(..) )
import Data.Eq                 ( Eq )
import Data.Maybe              ( isJust )
import Data.Typeable           ( Typeable )
import Prelude                 ( Integer )
import System.IO               ( IO )

-- from base-unicode-symbols
import Data.Function.Unicode   ( (∘) )

-- from concurrent-extra
import           Control.Concurrent.Broadcast ( Broadcast )
import qualified Control.Concurrent.Broadcast as Broadcast
    ( new, newWritten, read, tryRead, readTimeout, write, clear )


-------------------------------------------------------------------------------
-- Events
-------------------------------------------------------------------------------

-- | An event is in one of two possible states: \"Set\" or \"Cleared\".
newtype Event = Event {evBroadcast ∷ Broadcast ()} deriving (Eq, Typeable)

-- | Create an event in the \"Cleared\" state.
new ∷ IO Event
new = Event <$> Broadcast.new

-- | Create an event in the \"Set\" state.
newSet ∷ IO Event
newSet = Event <$> Broadcast.newWritten ()

{-| Block until the event is 'set'.

If the state of the event is already \"Set\" this function will return
immediately. Otherwise it will block until another thread calls 'set'.

You can also stop a thread that is waiting for an event by throwing an
asynchronous exception.
-}
wait ∷ Event → IO ()
wait = Broadcast.read ∘ evBroadcast

{-| Block until the event is 'set' or until a timer expires.

Like 'wait', but with a timeout. A return value of 'False' indicates a timeout
occurred.

The timeout is specified in microseconds. A timeout of 0 &#x3bc;s will cause the
function to return 'False' without blocking in case the event state is
\"Cleared\". Negative timeouts are treated the same as a timeout of 0 &#x3bc;s.
-}
waitTimeout ∷ Event → Integer → IO Bool
waitTimeout ev time = isJust <$> Broadcast.readTimeout (evBroadcast ev) time

{-| Changes the state of the event to \"Set\". All threads that where waiting
for this event are woken. Threads that 'wait' after the state is changed to
\"Set\" will not block at all.
-}
set ∷ Event → IO ()
set ev = Broadcast.write (evBroadcast ev) ()

{-| Changes the state of the event to \"Cleared\". Threads that 'wait' after the
state is changed to \"Cleared\" will block until the state is changed to \"Set\".
-}
clear ∷ Event → IO ()
clear = Broadcast.clear ∘ evBroadcast

{-| Returns 'True' if the state of the event is \"Set\" and 'False' if the state
is \"Cleared\".

Notice that this is only a snapshot of the state. By the time a program reacts
on its result it may already be out of date.
-}
isSet ∷ Event → IO Bool
isSet = fmap isJust ∘ Broadcast.tryRead ∘ evBroadcast


-- The End ---------------------------------------------------------------------
