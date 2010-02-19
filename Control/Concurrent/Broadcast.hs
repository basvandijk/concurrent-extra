{-# LANGUAGE DeriveDataTypeable, NoImplicitPrelude, UnicodeSyntax #-}

-------------------------------------------------------------------------------
-- |
-- Module     : Control.Concurrent.Broadcast
-- Copyright  : (c) 2010 Bas van Dijk & Roel van Dijk
-- License    : BSD3 (see the file LICENSE)
-- Maintainer : Bas van Dijk <v.dijk.bas@gmail.com>
--            , Roel van Dijk <vandijk.roel@gmail.com>
--
-- @
-- import           Control.Concurrent.Broadcast              ( Broadcast )
-- import qualified Control.Concurrent.Broadcast as Broadcast ( ... )
-- @
--
-------------------------------------------------------------------------------

module Control.Concurrent.Broadcast
  ( Broadcast
  , new
  , read
  , tryRead
  , readTimeout
  , write
  , clear
  ) where


-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

-- from base:
import Control.Applicative     ( (<$>) )
import Control.Arrow           ( first, second )
import Control.Monad           ( (>>=), (>>), return, fmap, forM_, fail )
import Control.Concurrent.MVar ( MVar, newMVar, newEmptyMVar
                               , takeMVar, putMVar, readMVar, modifyMVar_
                               )
import Control.Exception       ( block, unblock )
import Data.Eq                 ( Eq )
import Data.Function           ( ($), const )
import Data.Int                ( Int )
import Data.List               ( delete )
import Data.Maybe              ( Maybe(Nothing, Just) )
import Data.Ord                ( Ord, max )
import Data.Tuple              ( fst )
import Data.Typeable           ( Typeable )
import Prelude                 ( fromInteger )
import System.IO               ( IO )
import System.Timeout          ( timeout )

-- from base-unicode-symbols:
import Data.Function.Unicode   ( (∘) )

-- from concurrent-extra:
import Utils ( purelyModifyMVar )


-------------------------------------------------------------------------------
-- Broadcast
-------------------------------------------------------------------------------

newtype Broadcast α = Broadcast {unBroadcast ∷ MVar (Maybe α, [MVar α])}
    deriving (Eq, Typeable)

new ∷ IO (Broadcast α)
new = Broadcast <$> newMVar (Nothing, [])

read ∷ Broadcast α → IO α
read (Broadcast mv) = block $ do
  t@(mx, ls) ← takeMVar mv
  case mx of
    Nothing → do l ← newEmptyMVar
                 putMVar mv (mx, l:ls)
                 takeMVar l
    Just x  → do putMVar mv t
                 return x

tryRead ∷ Broadcast α → IO (Maybe α)
tryRead = fmap fst ∘ readMVar ∘ unBroadcast

readTimeout ∷ Broadcast α → Int → IO (Maybe α)
readTimeout (Broadcast mv) time = block $ do
  t@(mx, ls) ← takeMVar mv
  case mx of
    Nothing → do l ← newEmptyMVar
                 putMVar mv (mx, l:ls)
                 my ← unblock $ timeout (max time 0) (takeMVar l)
                 case my of
                   Nothing → do purelyModifyMVar mv $ second $ delete l
                                return my
                   Just _  → return my
    Just _  → do putMVar mv t
                 return mx

write ∷ Broadcast α → α → IO ()
write (Broadcast mv) x =
    modifyMVar_ mv $ \(_, ls) → do
      forM_ ls $ \l → putMVar l x
      return (Just x, [])

clear ∷ Broadcast α → IO ()
clear (Broadcast mv) = purelyModifyMVar mv $ first $ const Nothing


-- The End ---------------------------------------------------------------------


