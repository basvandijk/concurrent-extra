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
import Control.Monad               ( return, (>>=), fail )
import Control.Concurrent.STM      ( STM, retry )
import Control.Concurrent.STM.TVar ( TVar, newTVar, readTVar, writeTVar )
import Data.Eq                     ( Eq )
import Data.Function               ( ($) )
import Data.Maybe                  ( Maybe(Nothing, Just) )
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

read ∷ Broadcast α → STM α
read (Broadcast tv) = do
  st ← readTVar tv
  case st of
    Just x  → return x
    Nothing → retry

tryRead ∷ Broadcast α → STM (Maybe α)
tryRead = readTVar ∘ unBroadcast  

write ∷ Broadcast α → α → STM ()
write (Broadcast tv) x = writeTVar tv $ Just x

clear ∷ Broadcast α → STM ()
clear (Broadcast tv) = writeTVar tv Nothing
 

-- The End ---------------------------------------------------------------------
