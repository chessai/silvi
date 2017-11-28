{ mkDerivation, aeson, attoparsec, base, bytestring, fetchgit
, hashable, HUnit, primitive, QuickCheck, stdenv, test-framework
, test-framework-hunit, test-framework-quickcheck2, text, torsor
, vector
}:
mkDerivation {
  pname = "chronos";
  version = "1.0";
  src = fetchgit {
    url = "https://github.com/chessai/chronos.git";
    sha256 = "11ykr6lk9s56n81dq5a2h19dxp1b5c067lj04f4jih43scmbfzwm";
    rev = "f5f76b26e3dcde07b66533bbaf84024e5248ee9a";
  };
  libraryHaskellDepends = [
    aeson attoparsec base bytestring hashable primitive text torsor
    vector
  ];
  testHaskellDepends = [
    attoparsec base bytestring HUnit QuickCheck test-framework
    test-framework-hunit test-framework-quickcheck2 text
  ];
  homepage = "https://github.com/andrewthad/chronos#readme";
  description = "A performant time library";
  license = stdenv.lib.licenses.bsd3;
}
