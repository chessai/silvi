{ mkDerivation, base, base-orphans, deepseq, fetchgit, ghc-prim
, HUnit, primitive, QuickCheck, random, semigroups, stdenv
, template-haskell, test-framework, test-framework-hunit
, test-framework-quickcheck2, transformers
}:
mkDerivation {
  pname = "vector";
  version = "0.12.0.1";
  src = fetchgit {
    url = "https://github.com/haskell/vector.git";
    sha256 = "1362fvfs18rwglnaavhs9s6knrk49gs46fmp3jl0wlfwv3wwq40p";
    rev = "7a2f1e33c5eb4d29c724bee1926e9d5f7feeee15";
  };
  libraryHaskellDepends = [
    base deepseq ghc-prim primitive semigroups
  ];
  testHaskellDepends = [
    base base-orphans HUnit primitive QuickCheck random
    template-haskell test-framework test-framework-hunit
    test-framework-quickcheck2 transformers
  ];
  configureFlags = "-f unsafechecks"; 
  homepage = "https://github.com/haskell/vector";
  description = "Efficient Arrays";
  license = stdenv.lib.licenses.bsd3;
}
