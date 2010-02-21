{-# LANGUAGE NoImplicitPrelude
           , UnicodeSyntax
  #-}

module Control.Concurrent.Thread.Test ( tests ) where


-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

-- from base:
import Control.Concurrent ( )

-- from base-unicode-symbols:
import Prelude.Unicode       ( )

-- from concurrent-extra:
import qualified Control.Concurrent.Thread as Thread ( )
import TestUtils ( )

-- from HUnit:
import Test.HUnit (  )

-- from test-framework:
import Test.Framework  ( Test )

-- from test-framework-hunit:
import Test.Framework.Providers.HUnit ( )


-------------------------------------------------------------------------------
-- Tests for Thread
-------------------------------------------------------------------------------

tests ∷ [Test]
tests = []


-- The End ---------------------------------------------------------------------
