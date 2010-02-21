{-# LANGUAGE DeriveDataTypeable, NoImplicitPrelude, UnicodeSyntax #-}

-------------------------------------------------------------------------------
-- |
-- Module     : Control.Concurrent.Broadcast
-- Copyright  : (c) 2010 Bas van Dijk & Roel van Dijk
-- License    : BSD3 (see the file LICENSE)
-- Maintainer : Bas van Dijk <v.dijk.bas@gmail.com>
--            , Roel van Dijk <vandijk.roel@gmail.com>
--
-- A Broadcast variable is a mechanism for communication between
-- threads. Multiple reader threads can wait until a broadcaster thread writes a
-- signal. The readers block until the signal is received. When the broadcaster
-- sends the signal all readers are woken.
--
-- All functions are /exception safe/. Throwing asynchronous exceptions will not
-- compromise the internal state of a 'Broadcast' variable.
--
-- This module is designed to be imported qualified. We suggest importing it
-- like:
--
-- @
-- import           Control.Concurrent.Broadcast              ( Broadcast )
-- import qualified Control.Concurrent.Broadcast as Broadcast ( ... )
-- @
-------------------------------------------------------------------------------

module Control.Concurrent.Broadcast
  ( Broadcast
  , new
  , newWritten
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
import Control.Arrow           ( first )
import Control.Monad           ( (>>=), (>>), return, fmap, forM_, fail )
import Control.Concurrent.MVar ( MVar, newMVar, newEmptyMVar
                               , takeMVar, putMVar, readMVar, modifyMVar_
                               )
import Control.Exception       ( block, unblock )
import Data.Eq                 ( Eq )
import Data.Function           ( ($), const )
import Data.Int                ( Int )
import Data.List               ( delete, length )
import Data.Maybe              ( Maybe(Nothing, Just) )
import Data.Ord                ( Ord, max )
import Data.Tuple              ( fst )
import Data.Typeable           ( Typeable )
import Prelude                 ( fromInteger, seq )
import System.IO               ( IO )
import System.Timeout          ( timeout )

-- from base-unicode-symbols:
import Data.Function.Unicode   ( (∘) )

-- from concurrent-extra:
import Utils                   ( purelyModifyMVar )


-------------------------------------------------------------------------------
-- Broadcast
-------------------------------------------------------------------------------

-- | A broadcast variable. It can be thought of as a box, which may be empty of
-- full.
newtype Broadcast α = Broadcast {unBroadcast ∷ MVar (Maybe α, [MVar α])}
    deriving (Eq, Typeable)

-- | Create a new empty 'Broadcast' variable.
new ∷ IO (Broadcast α)
new = Broadcast <$> newMVar (Nothing, [])

-- | Create a new 'Broadcast' variable containing an initial value.
newWritten ∷ α → IO (Broadcast α)
newWritten x = Broadcast <$> newMVar (Just x, [])

{-| Read the value of a 'Broadcast' variable.

If the 'Broadcast' variable contains a value it will be returned immediately,
otherwise it will block until another thread 'write's a value to the 'Broadcast'
variable.
-}
read ∷ Broadcast α → IO α
read (Broadcast mv) = block $ do
  t@(mx, ls) ← takeMVar mv
  case mx of
    Nothing → do l ← newEmptyMVar
                 putMVar mv (mx, l:ls)
                 takeMVar l
    Just x  → do putMVar mv t
                 return x

{-| Try to read the value of a 'Broadcast' variable; non blocking.

Like 'read' but doesn't block. Returns 'Just' the contents of the 'Broadcast' if
it wasn't empty, 'Nothing' otherwise.
-}
tryRead ∷ Broadcast α → IO (Maybe α)
tryRead = fmap fst ∘ readMVar ∘ unBroadcast

{-| Read the value of a 'Broadcast' variable if it is available within a given
amount of time.

Like 'read', but with a timeout. A return value of 'Nothing' indicates a timeout
occurred.

The timeout is specified in microseconds.  A timeout of 0 &#x3bc;s will cause
the function to return 'Nothing' without blocking in case the 'Broadcast' was
empty. Negative timeouts are treated the same as a timeout of 0 &#x3bc;s. The
maximum timeout is constrained by the range of the 'Int' type. The Haskell
standard guarantees an upper bound of at least @2^29-1@ giving a maximum timeout
of at least @(2^29-1) / 10^6@ = ~536 seconds.
-}
readTimeout ∷ Broadcast α → Int → IO (Maybe α)
readTimeout (Broadcast mv) time = block $ do
  t@(mx, ls) ← takeMVar mv
  case mx of
    Nothing → do l ← newEmptyMVar
                 putMVar mv (mx, l:ls)
                 my ← unblock $ timeout (max time 0) (takeMVar l)
                 case my of
                   Nothing → do deleteReader l mv
                                return my
                   Just _  → return my
    Just _  → do putMVar mv t
                 return mx

{-| Write a new value into a 'Broadcast' variable.

If the variable is empty any threads that are reading from the variable will be
woken. If the variable is full its contents will simply be overwritten.
-}
write ∷ Broadcast α → α → IO ()
write (Broadcast mv) x =
    modifyMVar_ mv $ \(_, ls) → do
      forM_ ls $ \l → putMVar l x
      return (Just x, [])

-- | Clear the contents of a 'Broadcast' variable.
clear ∷ Broadcast α → IO ()
clear (Broadcast mv) = purelyModifyMVar mv $ first $ const Nothing


-------------------------------------------------------------------------------

deleteReader ∷ MVar α → MVar (Maybe α, [MVar α]) → IO ()
deleteReader l mv = do 
  (mx, ls) ← takeMVar mv
  let ls' = delete l ls
  length ls' `seq` putMVar mv (mx, ls')


-- The End ---------------------------------------------------------------------

