{ mkDerivation, aeson, attoparsec, base, bytestring, clock
, fetchgit, hashable, HUnit, primitive, QuickCheck, stdenv
, test-framework, test-framework-hunit, test-framework-quickcheck2
, text, torsor, vector
}:
mkDerivation {
  pname = "chronos";
  version = "1.0.2";
  src = fetchgit {
    url = "https://github.com/chessai/chronos.git";
    sha256 = "1x26cj0c31vznk6xsagsb2pbf6cvbjxrpp091kfdgi8s9rnnrfzw";
    rev = "189743f8cdb03b2c83e9fd81fe25d07c1bb17358";
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
