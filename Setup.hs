#! /usr/bin/env runhaskell

{-# LANGUAGE NoImplicitPrelude, UnicodeSyntax #-}

module Main (main) where

-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

-- from base
import Control.Monad       ( (>>), return )
import Data.Bool           ( Bool )
import System.Cmd          ( system )
import System.FilePath     ( (</>) )
import System.IO           ( IO )

-- from cabal
import Distribution.Simple ( defaultMainWithHooks
                           , simpleUserHooks
                           , UserHooks(runTests)
                           , Args
                           )

import Distribution.Simple.LocalBuildInfo ( LocalBuildInfo(..) )
import Distribution.PackageDescription    ( PackageDescription(..) )

-------------------------------------------------------------------------------

main ∷ IO ()
main = defaultMainWithHooks hooks
  where
    hooks = simpleUserHooks {runTests = runTests'}

-- Run a 'test' binary that gets built when configured with '-ftest'.
runTests' ∷ Args → Bool → PackageDescription → LocalBuildInfo → IO ()
runTests' _ _ _ _ = system testcmd >> return ()
  where testcmd = "." </> "dist" </> "build" </> "test" </> "test"
