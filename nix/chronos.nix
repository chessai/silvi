{ mkDerivation, aeson, attoparsec, base, bytestring, fetchgit
, hashable, HUnit, primitive, QuickCheck, stdenv, test-framework
, test-framework-hunit, test-framework-quickcheck2, text, vector
}:
mkDerivation {
  pname = "chronos";
  version = "0.4";
  src = fetchgit {
    url = "https://github.com/chessai/chronos.git";
    sha256 = "18m0zfxcsfaaw2gjkcpc6dk2x0c32wg48w90vaqw8szmakcjy8an";
    rev = "844609be548c6a8b8bd2427a5a529946f72c7cc3";
  };
  libraryHaskellDepends = [
    aeson attoparsec base bytestring hashable primitive text vector
  ];
  testHaskellDepends = [
    attoparsec base bytestring HUnit QuickCheck test-framework
    test-framework-hunit test-framework-quickcheck2 text
  ];
  homepage = "https://github.com/andrewthad/chronos#readme";
  description = "A performant time library";
  license = stdenv.lib.licenses.bsd3;
}
