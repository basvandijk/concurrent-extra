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
-- import Control.Concurrent.FanOutChan ( FanOutChan, ReadOut )
-- import qualified Control.Concurrent.FanOutChan as FanOutChan ( ... )
-- @
-------------------------------------------------------------------------------

module Control.Concurrent.FanOutChan
    ( FanOutChan
    , new
    , ReadOut
    , newReadOut

      -- * Writing
    , write
    , unGet
    , writeList

      -- * Reading
    , read
    , isEmpty
    , getContents
    ) where


-------------------------------------------------------------------------------
-- Import
-------------------------------------------------------------------------------

-- from base:
import Control.Concurrent.Chan ( Chan, newChan, dupChan
                               , writeChan, unGetChan, writeList2Chan
                               , readChan, isEmptyChan, getChanContents
                               )
import Data.Bool               ( Bool )
import Data.Functor            ( (<$>), fmap )
import System.IO               ( IO )

-- from base-unicode-symbols:
import Data.Function.Unicode ( (∘) )


-------------------------------------------------------------------------------
-- Fan out channels
-------------------------------------------------------------------------------

newtype FanOutChan α = FanOutChan {unFanOutChan ∷ Chan α}

new ∷ IO (FanOutChan α)
new = FanOutChan <$> newChan

newtype ReadOut α = ReadOut {unReadOut ∷ Chan α}

newReadOut ∷ FanOutChan α → IO (ReadOut α)
newReadOut = fmap ReadOut ∘ dupChan ∘ unFanOutChan

write ∷ FanOutChan α → α → IO ()
write = writeChan ∘ unFanOutChan

unGet ∷ FanOutChan α → α → IO ()
unGet = unGetChan ∘ unFanOutChan

writeList ∷ FanOutChan α → [α] → IO ()
writeList = writeList2Chan ∘ unFanOutChan

read ∷ ReadOut α → IO α
read = readChan ∘ unReadOut

isEmpty ∷ ReadOut α → IO Bool
isEmpty = isEmptyChan ∘ unReadOut

getContents ∷ ReadOut α → IO [α]
getContents = getChanContents ∘ unReadOut


-- The End ---------------------------------------------------------------------
