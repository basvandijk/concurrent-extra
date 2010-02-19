{-# LANGUAGE DeriveDataTypeable
           , NoImplicitPrelude
           , UnicodeSyntax
  #-}

-------------------------------------------------------------------------------
-- |
-- Module     : Control.Concurrent.Thread
-- Copyright  : (c) 2010 Bas van Dijk & Roel van Dijk
-- License    : BSD3 (see the file LICENSE)
-- Maintainer : Bas van Dijk <v.dijk.bas@gmail.com>
--            , Roel van Dijk <vandijk.roel@gmail.com>
--
-------------------------------------------------------------------------------

module Control.Concurrent.Thread
  ( ThreadId
  , threadId
  , forkIO
  , forkOS
  , wait
  , waitTimeout
  , isRunning
  , killThread
  , tryKillThread
  , throwTo
  ) where


-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

-- from base:
import Control.Applicative ( (<$>) )
import Control.Exception   ( Exception, SomeException
                           , catch, block, unblock
                           )
import Control.Monad       ( return, (>>=), fail, (>>), fmap )
import Data.Bool           ( Bool )
import Data.Eq             ( Eq )
import Data.Function       ( ($), on )
import Data.Int            ( Int )
import Data.Maybe          ( Maybe(Nothing, Just), isNothing, isJust )
import Data.Ord            ( Ord, compare )
import Data.Typeable       ( Typeable )
import System.IO           ( IO )

import qualified Control.Concurrent as Conc ( ThreadId
                                            , forkIO, forkOS
                                            , throwTo, killThread
                                            )

-- from base-unicode-symbols:
import Data.Function.Unicode ( (∘) )

-- from concurrent-extra:
import           Control.Concurrent.Broadcast ( Broadcast )
import qualified Control.Concurrent.Broadcast as Broadcast
    ( new, write, read, tryRead, readTimeout )

import Utils ( void )


-------------------------------------------------------------------------------
-- Threads
-------------------------------------------------------------------------------

data ThreadId = ThreadId { stopped   ∷ Broadcast (Maybe SomeException)
                         , threadId  ∷ Conc.ThreadId
                         } deriving (Eq, Typeable)

instance Ord ThreadId where
    compare = compare `on` threadId

fork ∷ (IO () → IO Conc.ThreadId) → IO () → IO ThreadId
fork f a = do
  stp ← Broadcast.new
  tid ← f $ block
          $ catch (unblock a >> Broadcast.write stp Nothing)
                  (Broadcast.write stp ∘ Just)
  return $ ThreadId stp tid

forkIO ∷ IO () → IO ThreadId
forkIO = fork Conc.forkIO

forkOS ∷ IO () → IO ThreadId
forkOS = fork Conc.forkOS

wait ∷ ThreadId → IO (Maybe SomeException)
wait = Broadcast.read ∘ stopped

waitTimeout ∷ ThreadId → Int → IO (Maybe (Maybe SomeException))
waitTimeout = Broadcast.readTimeout ∘ stopped

isRunning ∷ ThreadId → IO Bool
isRunning = fmap isNothing ∘ Broadcast.tryRead ∘ stopped


-------------------------------------------------------------------------------
-- Convenience functions
-------------------------------------------------------------------------------

killThread ∷ ThreadId → IO ()
killThread t = do Conc.killThread $ threadId t
                  void $ wait t

tryKillThread ∷ ThreadId → Int → IO Bool
tryKillThread t time = do Conc.killThread $ threadId t
                          isJust <$> waitTimeout t time

throwTo ∷ Exception e ⇒ ThreadId → e → IO ()
throwTo = Conc.throwTo ∘ threadId


-- The End ---------------------------------------------------------------------
