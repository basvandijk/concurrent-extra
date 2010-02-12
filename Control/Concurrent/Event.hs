{-# LANGUAGE CPP, NoImplicitPrelude, UnicodeSyntax #-}

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
-- Each event has an internal 'State' which is either 'Set' or 'Cleared'. This
-- state can be changed with the corresponding functions 'set' and 'clear'. The
-- 'wait' function blocks until the state is 'Set'. An importand property of
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
  , State(..)
  , new
  , wait
  , waitTimeout
  , set
  , clear
  , state
  ) where

-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

-- from base
import Control.Applicative     ( (<$>) )
import Control.Arrow           ( first, second )
import Control.Monad           ( (>>=), (>>), return, fmap, forM_, fail )
import Control.Concurrent.MVar ( MVar, newMVar
                               , takeMVar, putMVar, readMVar, modifyMVar_
                               )
import Control.Exception       ( block, unblock )
import Data.Bool               ( Bool(..) )
import Data.Eq                 ( Eq )
import Data.Function           ( ($), const )
import Data.Int                ( Int )
import Data.Maybe              ( isJust )
import Data.Ord                ( max )
import Data.Tuple              ( fst )
import Prelude                 ( Enum, fromInteger )
import System.IO               ( IO )
import System.Timeout          ( timeout )
import Text.Read               ( Read )
import Text.Show               ( Show )

-- from base-unicode-symbols
import Data.Function.Unicode   ( (∘) )

-- from concurrent-extra
import           Control.Concurrent.Lock         ( Lock )
import qualified Control.Concurrent.Lock as Lock ( newAcquired
                                                 , acquire, release
                                                 )


-------------------------------------------------------------------------------
-- Events
-------------------------------------------------------------------------------

data State = Cleared | Set deriving (Enum, Eq, Show, Read)

-- | An event is in one of two possible states: 'Set' or 'Cleared'.
newtype Event = Event {unEvent ∷ (MVar (State, [Lock]))}

-- | Create an event. The initial state is 'Cleared'.
new ∷ IO Event
new = Event <$> newMVar (Cleared, [])

-- | Block until the event is 'set'.
--
-- If the state of the event is already 'Set' this function will return
-- immediately. Otherwise it will block until another thread calls 'set'.
--
-- You can also stop a thread that is waiting for an event by throwing an
-- asynchronous exception.
wait ∷ Event → IO ()
wait (Event mv) = block $ do
  t@(st, _) ← takeMVar mv
  case st of
    Set     → putMVar mv t
    Cleared → do l ← Lock.newAcquired
                 putMVar mv $ second (l:) t
                 unblock $ Lock.acquire l

-- | Block until the event is 'set' or until a timer expires.
--
-- Like 'wait' but with a timeout. A return value of 'False' indicates a timeout
-- occurred.
--
-- The timeout is specified in microseconds. A timeout of 0 &#x3bc;s will cause
-- the function to return 'False' without blocking in case the event state is
-- 'Cleared'. Negative timeouts are treated the same as a timeout of 0
-- &#x3bc;s. The maximum timeout is constrained by the range of the 'Int'
-- type. The Haskell standard guarantees an upper bound of at least @2^29-1@
-- giving a maximum timeout of at least @(2^29-1) / 10^6@ = ~536 seconds.
waitTimeout ∷ Event → Int → IO Bool
waitTimeout (Event mv) time = block $ do
  t@(st, _) ← takeMVar mv
  case st of
    Set     → do putMVar mv t 
                 return True
    Cleared → do l ← Lock.newAcquired
                 putMVar mv $ second (l:) t
                 unblock $ isJust <$> timeout (max time 0) (Lock.acquire l)

-- | Changes the state of the event to 'Set'. All threads that where 'wait'ing
-- for this event are woken. Threads that 'wait' after the state is changed to
-- 'Set' will not block at all.
set ∷ Event → IO ()
set (Event mv) = block $ do
  (_, ls) ← takeMVar mv
  forM_ ls Lock.release
  putMVar mv (Set, [])

-- | Changes the state of the event to 'Cleared'. Threads that 'wait' after the
-- state is changed to 'Cleared' will block util the state is changed to 'Set'.
clear ∷ Event → IO ()
clear (Event mv) = modifyMVar_ mv $ return ∘ first (const Cleared)

-- | Determines the current state of the event. 
--
-- Notice that this is only a snapshot of the state. By the time a program
-- reacts on its result it may already be out of date. This can be avoided by
-- synchronizing access to the event between threads.
state ∷ Event → IO State
state = fmap fst ∘ readMVar ∘ unEvent
