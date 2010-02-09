{-# LANGUAGE NoImplicitPrelude, UnicodeSyntax #-}

module Control.Concurrent.Event
  ( Event
  , new
  , isSet
  , wait
  , waitTimeout
  , set
  , clear
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
import Data.Bool               ( Bool(False, True), otherwise )
import Data.Function           ( ($), const )
import Data.Int                ( Int )
import Data.Maybe              ( isJust )
import Data.Tuple              ( fst )
import Prelude                 ( fromInteger )
import System.IO               ( IO )
import System.Timeout          ( timeout )

-- from base-unicode-symbols
import Data.Ord.Unicode        ( (≤) )
import Data.Function.Unicode   ( (∘) )

-- from concurrent-extra
import           Control.Concurrent.Lock         ( Lock )
import qualified Control.Concurrent.Lock as Lock ( newAcquired
                                                 , acquire, release
                                                 )


-------------------------------------------------------------------------------
-- Events
-------------------------------------------------------------------------------

newtype Event = Event {unEvent ∷ (MVar (Bool, [Lock]))}

new ∷ IO Event
new = Event <$> newMVar (False, [])

isSet ∷ Event → IO Bool
isSet = fmap fst ∘ readMVar ∘ unEvent

wait ∷ Event → IO ()
wait (Event mv) = block $ do
  t@(flag, _) ← takeMVar mv
  if flag
    then putMVar mv t
    else do l ← Lock.newAcquired
            putMVar mv $ second (l:) t
            unblock $ Lock.acquire l

waitTimeout ∷ Event → Int → IO Bool
waitTimeout (Event mv) time
    | time ≤ 0  = return True
    | otherwise = block $ do
  t@(flag, _) ← takeMVar mv
  if flag
    then putMVar mv t >> return flag
    else do l ← Lock.newAcquired
            putMVar mv $ second (l:) t
            unblock $ isJust <$> timeout time (Lock.acquire l)

set ∷ Event → IO ()
set (Event mv) = block $ do
  (_, ls) ← takeMVar mv
  forM_ ls Lock.release
  putMVar mv (True, [])

clear ∷ Event → IO ()
clear (Event mv) = modifyMVar_ mv $ return ∘ first (const False)
