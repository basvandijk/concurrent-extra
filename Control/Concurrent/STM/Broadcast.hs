{-# LANGUAGE DeriveDataTypeable, NoImplicitPrelude, UnicodeSyntax #-}

-------------------------------------------------------------------------------
-- |
-- Module     : Control.Concurrent.STM.Broadcast
-- Copyright  : (c) 2010 Bas van Dijk & Roel van Dijk
-- License    : BSD3 (see the file LICENSE)
-- Maintainer : Bas van Dijk <v.dijk.bas@gmail.com>
--            , Roel van Dijk <vandijk.roel@gmail.com>
--
-- A Broadcast variable is a mechanism for communication between
-- threads. Multiple reader threads can wait until a broadcaster thread writes a
-- signal. The readers retry until the signal is received. When the broadcaster
-- sends the signal all readers are woken.
--
-- This module is designed to be imported qualified. We suggest importing it
-- like:
--
-- @
-- import           Control.Concurrent.STM.Broadcast ( Broadcast )
-- import qualified Control.Concurrent.STM.Broadcast as Broadcast ( ... )
-- @
--
-------------------------------------------------------------------------------

module Control.Concurrent.STM.Broadcast
  ( Broadcast
  , new
  , newWritten
  , read
  , tryRead
  , write
  , clear
  ) where


-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

-- from base
import Control.Applicative         ( (<$>) )
import Control.Monad               ( return, (=<<), fmap )
import Control.Concurrent.STM      ( STM, retry )
import Control.Concurrent.STM.TVar ( TVar, newTVar, readTVar, writeTVar )
import Data.Eq                     ( Eq )
import Data.Maybe                  ( Maybe(Nothing, Just), maybe )
import Data.Typeable               ( Typeable )

-- from base-unicode-symbols
import Data.Function.Unicode       ( (∘) )


-------------------------------------------------------------------------------
-- Broadcast
-------------------------------------------------------------------------------

-- | A broadcast variable. It can be thought of as a box, which may be empty of
-- full.
newtype Broadcast α = Broadcast {unBroadcast ∷ TVar (Maybe α)}
    deriving (Eq, Typeable)

-- | Create a new empty 'Broadcast' variable.
new ∷ STM (Broadcast α)
new = Broadcast <$> newTVar Nothing

-- | Create a new 'Broadcast' variable containing an initial value.
newWritten ∷ α → STM (Broadcast α)
newWritten = fmap Broadcast ∘ newTVar ∘ Just

{-| Read the value of a 'Broadcast' variable.

If the 'Broadcast' variable contains a value it will be returned immediately,
otherwise it will retry until another thread 'write's a value to the 'Broadcast'
variable.
-}
read ∷ Broadcast α → STM α
read = (maybe retry return =<<) ∘ readTVar ∘ unBroadcast

{-| Try to read the value of a 'Broadcast' variable; non blocking.

Like 'read' but doesn't retry. Returns 'Just' the contents of the 'Broadcast' if
it wasn't empty, 'Nothing' otherwise.
-}
tryRead ∷ Broadcast α → STM (Maybe α)
tryRead = readTVar ∘ unBroadcast

{-| Write a new value into a 'Broadcast' variable.

If the variable is empty any threads that are reading from the variable will be
woken. If the variable is full its contents will simply be overwritten.
-}
write ∷ Broadcast α → α → STM ()
write b = writeTVar (unBroadcast b) ∘ Just

-- | Clear the contents of a 'Broadcast' variable.
clear ∷ Broadcast α → STM ()
clear b = writeTVar (unBroadcast b) Nothing


-- The End ---------------------------------------------------------------------
