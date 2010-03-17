{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude, BangPatterns #-}

-------------------------------------------------------------------------------
-- |
-- Module     : Control.Concurrent.FanOutChan
-- Copyright  : (c) 2010 Bas van Dijk & Roel van Dijk
-- License    : BSD3 (see the file LICENSE)
-- Maintainer : Bas van Dijk <v.dijk.bas@gmail.com>
--            , Roel van Dijk <vandijk.roel@gmail.com>
--
-- Channels with multiple independent read outs.
--
-- @
-- import           Control.Concurrent.FanOutChan ( FanOutChan, ReadOut )
-- import qualified Control.Concurrent.FanOutChan as FanOutChan ( ... )
-- @
-------------------------------------------------------------------------------

module Control.Concurrent.FanOutChan
    ( FanOutChan
    , new
    , ReadOut
    , newReadOut
    , write
    , read
    ) where


-------------------------------------------------------------------------------
-- Import
-------------------------------------------------------------------------------

-- from base:
import Control.Concurrent.Chan ( Chan, newChan, writeChan, readChan )
import Control.Concurrent.MVar ( MVar, newMVar
                               , withMVar, modifyMVar, modifyMVar_
                               )
import Control.Monad           ( return, (>>=), (>>), fail )
import Data.Function           ( ($), flip )
import Data.Functor            ( (<$>) )
import Data.Foldable           ( traverse_ )
import Data.Tuple              ( snd )
import System.IO               ( IO )
import System.Mem.Weak         ( addFinalizer )
import Prelude                 ( Integer, fromInteger, succ )

-- from base-unicode-symbols:
import Data.Function.Unicode ( (∘) )

-- from containers:
import           Data.Map        ( Map )
import qualified Data.Map as Map ( empty, insert, delete )


-------------------------------------------------------------------------------
-- Fan out channels
-------------------------------------------------------------------------------

newtype FanOutChan α = FanOutChan (MVar ( Integer -- source of unique values
                                        , Map Integer (Chan α)
                                        )
                                  )

new ∷ IO (FanOutChan α)
new = FanOutChan <$> newMVar (0, Map.empty)

data ReadOut α = ReadOut KeyToWeakPtr (Chan α)

type KeyToWeakPtr = Integer

newReadOut ∷ FanOutChan α → IO (ReadOut α)
newReadOut (FanOutChan mv) = do
  chan ← newChan

  unique ← modifyMVar mv $
    \(existing, m) → let !unique = succ existing
                         !m' = Map.insert unique chan m
                     in return ((unique, m'), unique)

  let keyToWeakPtr = succ unique ∷ KeyToWeakPtr

  addFinalizer keyToWeakPtr $ modifyMVar_ mv $
    \(existing, m) → let !m' = Map.delete unique m
                     in return (existing, m')

  return $ ReadOut keyToWeakPtr chan

write ∷ FanOutChan α → α → IO ()
write (FanOutChan mv) x = withMVar mv $ traverse_ (flip writeChan x) ∘ snd

read ∷ ReadOut α → IO α
read (ReadOut _ chan) = readChan chan


-- The End ---------------------------------------------------------------------
