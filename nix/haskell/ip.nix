{ mkDerivation, aeson, attoparsec, base, bytestring, criterion
, doctest, hashable, HUnit, primitive, QuickCheck
, quickcheck-classes, stdenv, test-framework, test-framework-hunit
, test-framework-quickcheck2, text, vector
}:
mkDerivation {
  pname = "ip";
  version = "1.1.2";
  sha256 = "5201cba09422ea754cb7128332f3669bce79616963929c10e444ef2b335b729b";
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
