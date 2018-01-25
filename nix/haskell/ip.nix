{ mkDerivation, aeson, attoparsec, base, bytestring, criterion
, doctest, fetchgit, hashable, HUnit, primitive, QuickCheck
, quickcheck-classes, stdenv, test-framework, test-framework-hunit
, test-framework-quickcheck2, text, vector
}:
mkDerivation {
  pname = "ip";
  version = "1.1.2";
  src = fetchgit {
    url = "https://github.com/andrewthad/haskell-ip.git";
    sha256 = "10j5dsyysz6im0bl05q1l05i1r02zws3rll1fvsfyqb880b0l4rl";
    rev = "f77cda50027cda9c7efc21005c5bc489287b82e9";
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
