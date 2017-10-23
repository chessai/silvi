{ mkDerivation, acme-cutegirl, aeson, attoparsec, base, bytestring
, fetchgit, hashable, HUnit, primitive, QuickCheck, stdenv
, test-framework, test-framework-hunit, test-framework-quickcheck2
, text, vector
}:
mkDerivation {
  pname = "chronos";
  version = "0.5";
  src = fetchgit {
    url = "https://github.com/chessai/chronos.git";
    sha256 = "188njqhyy1x73x2inflx2ql18009hh5xmbb7cmbsfmcanfhjblrd";
    rev = "d631fab1749644887e3b7c3026e631cb6e42174f";
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
