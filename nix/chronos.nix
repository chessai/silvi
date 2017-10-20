{ mkDerivation, acme-cutegirl, aeson, attoparsec, base, bytestring
, fetchgit, hashable, HUnit, primitive, QuickCheck, stdenv
, test-framework, test-framework-hunit, test-framework-quickcheck2
, text, vector
}:
mkDerivation {
  pname = "chronos";
  version = "0.4";
  src = fetchgit {
    url = "https://github.com/chessai/chronos.git";
    sha256 = "1r7b7ypg2kv7qllkcf93vfkzrk1afxplcsya77l2saz58ijrak1s";
    rev = "baba1ab62d247ed44c6eaa97a9991f8be74f4432";
  };
  libraryHaskellDepends = [
    aeson attoparsec base bytestring hashable primitive text vector
  ];
  testHaskellDepends = [
    acme-cutegirl attoparsec base bytestring HUnit QuickCheck
    test-framework test-framework-hunit test-framework-quickcheck2 text
  ];
  homepage = "https://github.com/andrewthad/chronos#readme";
  description = "A performant time library";
  license = stdenv.lib.licenses.bsd3;
}
