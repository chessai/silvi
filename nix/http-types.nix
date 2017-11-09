{ mkDerivation, array, base, bytestring, case-insensitive, doctest
, fetchgit, hspec, QuickCheck, quickcheck-instances, stdenv, text
}:
mkDerivation {
  pname = "http-types";
  version = "0.10";
  src = fetchgit {
    url = "https://github.com/chessai/http-types.git";
    sha256 = "1dd69s8zsbfdfgr0bzmxp2n1faxcm1q5rh23rhvkq0swny858yc7";
    rev = "a180dee4c40d6d1fed38e21602d8e1cb03244e01";
  };
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
