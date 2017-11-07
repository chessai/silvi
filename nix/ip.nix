{ mkDerivation, aeson, attoparsec, base, bytestring, criterion
, doctest, fetchgit, hashable, HUnit, primitive, QuickCheck
, quickcheck-classes, stdenv, test-framework, test-framework-hunit
, test-framework-quickcheck2, text, vector
}:
mkDerivation {
  pname = "ip";
  version = "1.1.0";
  src = fetchgit {
    url = "https://github.com/andrewthad/haskell-ip.git";
    sha256 = "10a1yprrwazcgq4xk0dmd1fq7jg5r7g9vwn18nmkjxrf4b01rn91";
    rev = "866ec75a1eb57c2d75925efd1a467cbd69b9e8c7";
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
  doCheck = false;
  homepage = "https://github.com/andrewthad/haskell-ip#readme";
  description = "Library for IP and MAC addresses";
  license = stdenv.lib.licenses.bsd3;
}
