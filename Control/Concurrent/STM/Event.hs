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
-- Each event has an internal 'State' which is either 'Set' or 'Cleared'. This
-- state can be changed with the corresponding functions 'set' and 'clear'. The
-- 'wait' function blocks until the state is 'Set'. An important property of
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
  , State(..)
  , new
  , wait
  , set
  , clear
  , state
  ) where

-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

-- from base
import Control.Applicative         ( (<$>) )
import Control.Monad               ( (>>=), fail, when )
import Control.Concurrent.STM      ( STM, retry )
import Control.Concurrent.STM.TVar ( TVar, newTVar, readTVar, writeTVar )
import Data.Eq                     ( Eq )
import Data.Function               ( ($) )
import Data.Typeable               ( Typeable )

-- from base-unicode-symbols
import Data.Eq.Unicode         ( (≡) )

-- from concurrent-extra
import Control.Concurrent.Event ( State(Cleared, Set) )


-------------------------------------------------------------------------------
-- Events
-------------------------------------------------------------------------------

-- | An event is in one of two possible states: 'Set' or 'Cleared'.
newtype Event = Event (TVar State) deriving (Eq, Typeable)

-- | Create an event. The initial state is 'Cleared'.
new ∷ STM Event
new = Event <$> newTVar Cleared

-- | Retry until the event is 'set'.
--
-- If the state of the event is already 'Set' this function will return
-- immediately. Otherwise it will retry until another thread calls 'set'.
--
-- You can also stop a thread that is waiting for an event by throwing an
-- asynchronous exception.
wait ∷ Event → STM ()
wait (Event tv) = do
  st ← readTVar tv
  when (st ≡ Cleared) retry

-- | Changes the state of the event to 'Set'. All threads that where waiting for
-- this event are woken. Threads that 'wait' after the state is changed to 'Set'
-- will not retry.
set ∷ Event → STM ()
set (Event tv) = do
  st ← readTVar tv
  when (st ≡ Cleared) $ writeTVar tv Set

-- | Changes the state of the event to 'Cleared'. Threads that 'wait' after the
-- state is changed to 'Cleared' will retry until the state is changed to 'Set'.
clear ∷ Event → STM ()
clear (Event tv) = do
  st ← readTVar tv
  when (st ≡ Set) $ writeTVar tv Cleared

-- | The current state of the event.
state ∷ Event → STM State
state (Event tv) = readTVar tv
