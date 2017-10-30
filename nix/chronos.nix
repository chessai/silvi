{ mkDerivation, aeson, attoparsec, base, bytestring, fetchgit
, hashable, HUnit, primitive, QuickCheck, stdenv, test-framework
, test-framework-hunit, test-framework-quickcheck2, text, torsor
, vector
}:
mkDerivation {
  pname = "chronos";
  version = "1.0";
  src = fetchgit {
    url = "https://github.com/andrewthad/chronos.git";
    sha256 = "1i7sl0nihp24nlnmz9p5m9v8533gbvzw856mwfiih6banz2w74lm";
    rev = "dfc7fe3c49e795bdff8376b6716906f172fa61e6";
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
