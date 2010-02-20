{-# LANGUAGE DeriveDataTypeable, NoImplicitPrelude, UnicodeSyntax #-}

-------------------------------------------------------------------------------
-- |
-- Module     : Control.Concurrent.STM.Event
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
-- import           Control.Concurrent.STM.Event          ( Event )
-- import qualified Control.Concurrent.STM.Event as Event ( ... )
-- @
--
-------------------------------------------------------------------------------

module Control.Concurrent.STM.Event
  ( Event

  , new
  , newSet

  , wait

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
import Control.Concurrent.STM  ( STM )
import Data.Bool               ( Bool )
import Data.Eq                 ( Eq )
import Data.Maybe              ( isJust )
import Data.Typeable           ( Typeable )

-- from base-unicode-symbols
import Data.Function.Unicode   ( (∘) )

-- from concurrent-extra
import           Control.Concurrent.STM.Broadcast ( Broadcast )
import qualified Control.Concurrent.STM.Broadcast as Broadcast
    ( new, newWritten, read, tryRead, write, clear )


-------------------------------------------------------------------------------
-- Events
-------------------------------------------------------------------------------

-- | An event is in one of two possible states: \"Set\" or \"Cleared\".
newtype Event = Event {evBroadcast ∷ Broadcast ()} deriving (Eq, Typeable)

-- | Create an event in the \"Cleared\" state.
new ∷ STM Event
new = Event <$> Broadcast.new

-- | Create an event in the \"Set\" state.
newSet ∷ STM Event
newSet = Event <$> Broadcast.newWritten ()

-- | Retry until the event is 'set'.
--
-- If the state of the event is already \"Set\" this function will return
-- immediately. Otherwise it will retry until another thread calls 'set'.
wait ∷ Event → STM ()
wait = Broadcast.read ∘ evBroadcast

-- | Changes the state of the event to \"Set\". All threads that where waiting for
-- this event are woken. Threads that 'wait' after the state is changed to \"Set\"
-- will not retry.
set ∷ Event → STM ()
set ev = Broadcast.write (evBroadcast ev) ()

-- | Changes the state of the event to \"Cleared\". Threads that 'wait' after the
-- state is changed to \"Cleared\" will retry until the state is changed to \"Set\".
clear ∷ Event → STM ()
clear = Broadcast.clear ∘ evBroadcast

isSet ∷ Event → STM Bool
isSet = fmap isJust ∘ Broadcast.tryRead ∘ evBroadcast


-- The End ---------------------------------------------------------------------
