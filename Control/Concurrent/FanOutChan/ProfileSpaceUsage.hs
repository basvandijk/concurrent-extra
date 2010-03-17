{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude #-}

-- The following program tests if ReadOuts are properly garbage collected.
--
-- 1) Compile this program with -prof
-- 2) Run it with +RTS -hy
-- 3) Apply hp2ps to the .hp file to create a postscript graph
-- 4) View the graph (using okular or what have you) to see the heap usage.

module Main where

-- from base:
import Control.Concurrent ( forkIO )
import Control.Monad      ( (>>=), (>>), fail, forever, replicateM_ )
import System.IO          ( IO, putStr, hSetBuffering, stdout, BufferMode(NoBuffering) )
import Data.Function      ( ($) )
import Prelude            ( fromInteger )

-- from concurrent-extra:
import Control.Concurrent.FanOutChan ( new, newReadOut, write, read )

main ∷ IO ()
main = do -- Because this program prints to stdout using putStr and stdout is
          -- LineBuffering by default we don't see anything if we don't set
          -- buffering to something other than LineBuffering:
          hSetBuffering stdout NoBuffering

          -- Create a new fan-out-channel:
          fanOut ← new

          -- Fork a thread that continually writes to the fan-out-channel as
          -- fast as it can:
          _ ← forkIO $ forever $ write fanOut "."

          -- Now continually (n times) create a read-out from the
          -- fan-out-channel and read from it:
          replicateM_ n $ do readOut ← newReadOut fanOut
                             s ← read readOut
                             putStr s
                             -- After this iteration 'readOut' is unreachable
                             -- and should be garbage collected!
    where
      n = 10000
