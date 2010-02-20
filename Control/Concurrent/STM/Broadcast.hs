{-# LANGUAGE DeriveDataTypeable, NoImplicitPrelude, UnicodeSyntax #-}

-------------------------------------------------------------------------------
-- |
-- Module     : Control.Concurrent.STM.Broadcast
-- Copyright  : (c) 2010 Bas van Dijk & Roel van Dijk
-- License    : BSD3 (see the file LICENSE)
-- Maintainer : Bas van Dijk <v.dijk.bas@gmail.com>
--            , Roel van Dijk <vandijk.roel@gmail.com>
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

newtype Broadcast α = Broadcast {unBroadcast ∷ TVar (Maybe α)}
    deriving (Eq, Typeable)

new ∷ STM (Broadcast α)
new = Broadcast <$> newTVar Nothing

newWritten ∷ α → STM (Broadcast α)
newWritten = fmap Broadcast ∘ newTVar ∘ Just

read ∷ Broadcast α → STM α
read = (maybe retry return =<<) ∘ readTVar ∘ unBroadcast

tryRead ∷ Broadcast α → STM (Maybe α)
tryRead = readTVar ∘ unBroadcast

write ∷ Broadcast α → α → STM ()
write b = writeTVar (unBroadcast b) ∘ Just

clear ∷ Broadcast α → STM ()
clear b = writeTVar (unBroadcast b) Nothing


-- The End ---------------------------------------------------------------------
