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
    sha256 = "1ygc3nhc5b4vin6qhm5a66cl3934vp5pq6c3pcn1zvs3a1yxgiin";
    rev = "44886f13833397622b844cd85f9073bc9f650804";
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
