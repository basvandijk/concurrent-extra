{ cabal, async, HUnit, random, stm, testFramework
, testFrameworkHunit, unboundedDelays
}:

cabal.mkDerivation (self: {
  pname = "concurrent-extra";
  version = "0.7.0.9";
  src = ./.;
  buildDepends = [ stm unboundedDelays ];
  testDepends = [
    async HUnit random stm testFramework testFrameworkHunit
    unboundedDelays
  ];
  meta = {
    homepage = "https://github.com/basvandijk/concurrent-extra";
    description = "Extra concurrency primitives";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
