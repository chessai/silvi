{ mkDerivation, aeson, attoparsec, base, bytestring, criterion
, doctest, fetchgit, hashable, HUnit, primitive, QuickCheck
, quickcheck-classes, stdenv, test-framework, test-framework-hunit
, test-framework-quickcheck2, text, vector
}:
mkDerivation {
  pname = "ip";
  version = "1.1.1";
  src = fetchgit {
    url = "https://github.com/andrewthad/haskell-ip.git";
    sha256 = "086hh02s9d4mz3dzx074c977asqw1y2ig05mjfwnb29ikfcg8xaf";
    rev = "a51b2fb3cdba458398f414744da9768f43b7b1d9";
  };
  libraryHaskellDepends = [
    aeson attoparsec base bytestring hashable primitive text vector
  ];
  testHaskellDepends = [
    attoparsec base bytestring doctest HUnit QuickCheck
    quickcheck-classes test-framework test-framework-hunit
    test-framework-quickcheck2 text
  ];
  benchmarkHaskellDepends = [
    attoparsec base bytestring criterion text
  ];
  homepage = "https://github.com/andrewthad/haskell-ip#readme";
  description = "Library for IP and MAC addresses";
  license = stdenv.lib.licenses.bsd3;
}
