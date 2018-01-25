{ mkDerivation, aeson, attoparsec, base, bytestring, clock
, fetchgit, hashable, HUnit, primitive, QuickCheck, stdenv
, test-framework, test-framework-hunit, test-framework-quickcheck2
, text, torsor, vector
}:
mkDerivation {
  pname = "chronos";
  version = "1.0.2";
  src = fetchgit {
    url = "https://github.com/andrewthad/chronos.git";
    sha256 = "02xw3x5h8dmkbv3qbv58svrnqaakipw33fxv748jqqlw1bwif0y2";
    rev = "8abfa643056c6dda3bfaa4535dfc6471fe4d5765";
  };
  libraryHaskellDepends = [
    aeson attoparsec base bytestring clock hashable primitive text
    torsor vector
  ];
  testHaskellDepends = [
    attoparsec base bytestring HUnit QuickCheck test-framework
    test-framework-hunit test-framework-quickcheck2 text
  ];
  homepage = "https://github.com/andrewthad/chronos#readme";
  description = "A performant time library";
  license = stdenv.lib.licenses.bsd3;
}
