{ mkDerivation, array, base, blaze-builder, bytestring
, case-insensitive, doctest, fetchgit, hspec, QuickCheck
, quickcheck-instances, stdenv, text
}:
mkDerivation {
  pname = "http-types";
  version = "0.9.1";
  src = fetchgit {
    url = "https://github.com/chessai/http-types.git";
    sha256 = "056k0xjirazk34v0hprq3r628h3hf2c5priqmxsxk961xg18frby";
    rev = "aa5944b01ddd69b82b67015eb8f2d94dcdaab315";
  };
  libraryHaskellDepends = [
    array base blaze-builder bytestring case-insensitive text
  ];
  testHaskellDepends = [
    base blaze-builder bytestring doctest hspec QuickCheck
    quickcheck-instances text
  ];
  homepage = "https://github.com/aristidb/http-types";
  description = "Generic HTTP types for Haskell (for both client and server code)";
  license = stdenv.lib.licenses.bsd3;
}
