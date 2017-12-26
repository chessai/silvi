{ mkDerivation, aeson, attoparsec, base, bytestring, clock
, fetchgit, hashable, HUnit, primitive, QuickCheck, stdenv
, test-framework, test-framework-hunit, test-framework-quickcheck2
, text, torsor, vector
}:
mkDerivation {
  pname = "chronos";
  version = "1.0.1";
  src = fetchgit {
    url = "https://github.com/andrewthad/chronos.git";
    sha256 = "151m28xx97yklfqsykwd4nsb8kzvzg4yrrr7w4gin69jv8pvswnk";
    rev = "87c40d22df69ec9a475ede5676bd362e12682db8";
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
