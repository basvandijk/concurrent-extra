{-# LANGUAGE NoImplicitPrelude
           , UnicodeSyntax
  #-}

module Control.Concurrent.ReadWriteVar.Test ( tests ) where


-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

-- from base:
import Control.Concurrent ( )

-- from base-unicode-symbols:
import Prelude.Unicode       ( )

-- from concurrent-extra:
import qualified Control.Concurrent.ReadWriteVar as RWVar ( )
import TestUtils ( )

-- from HUnit:
import Test.HUnit (  )

-- from test-framework:
import Test.Framework  ( Test )

-- from test-framework-hunit:
import Test.Framework.Providers.HUnit ( )


-------------------------------------------------------------------------------
-- Tests for ReadWriteVar
-------------------------------------------------------------------------------

tests ∷ [Test]
tests = []


-- The End ---------------------------------------------------------------------
