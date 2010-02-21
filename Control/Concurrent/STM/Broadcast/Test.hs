{-# LANGUAGE NoImplicitPrelude
           , UnicodeSyntax
  #-}

module Control.Concurrent.STM.Broadcast.Test ( tests ) where


-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

-- from base:
import Control.Concurrent ( )

-- from base-unicode-symbols:
import Prelude.Unicode       ( )

-- from concurrent-extra:
import qualified Control.Concurrent.STM.Broadcast as Broadcast ( )
import TestUtils ( )

-- from HUnit:
import Test.HUnit (  )

-- from test-framework:
import Test.Framework  ( Test )

-- from test-framework-hunit:
import Test.Framework.Providers.HUnit ( )


-------------------------------------------------------------------------------
-- Tests for STM Broadcast
-------------------------------------------------------------------------------

tests ∷ [Test]
tests = []


-- The End ---------------------------------------------------------------------
