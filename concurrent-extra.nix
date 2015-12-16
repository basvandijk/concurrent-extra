{ mkDerivation, async, base, HUnit, random, stdenv, stm
, test-framework, test-framework-hunit, unbounded-delays
}:
mkDerivation {
  pname = "concurrent-extra";
  version = "0.7.0.9";
  src = ./.;
  libraryHaskellDepends = [ base stm unbounded-delays ];
  testHaskellDepends = [
    async base HUnit random stm test-framework test-framework-hunit
    unbounded-delays
  ];
  homepage = "https://github.com/basvandijk/concurrent-extra";
  description = "Extra concurrency primitives";
  license = stdenv.lib.licenses.bsd3;
}
