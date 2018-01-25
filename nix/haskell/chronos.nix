{ mkDerivation, aeson, attoparsec, base, bytestring, clock
, hashable, HUnit, primitive, QuickCheck, stdenv, test-framework
, test-framework-hunit, test-framework-quickcheck2, text, torsor
, vector
}:
mkDerivation {
  pname = "chronos";
  version = "1.0.1";
  sha256 = "928de836002ca300d756912a800c53ac4ee520e4de4cda9449750673b59572ed";
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
