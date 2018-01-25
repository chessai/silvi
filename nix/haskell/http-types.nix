{ mkDerivation, array, base, bytestring, case-insensitive, doctest
, hspec, QuickCheck, quickcheck-instances, stdenv, text
}:
mkDerivation {
  pname = "http-types";
  version = "0.11";
  sha256 = "5ab39721547d44db2662efc98996a5af42e88dac58aa18a4806b9e185c068323";
  libraryHaskellDepends = [
    array base bytestring case-insensitive text
  ];
  testHaskellDepends = [
    base bytestring doctest hspec QuickCheck quickcheck-instances text
  ];
  homepage = "https://github.com/aristidb/http-types";
  description = "Generic HTTP types for Haskell (for both client and server code)";
  license = stdenv.lib.licenses.bsd3;
}
