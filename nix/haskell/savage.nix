{ mkDerivation, base, bytestring, containers, exceptions, fetchgit
, mmorph, monad-control, mtl, primitive, random, resourcet, stdenv
, text, time, transformers, transformers-base, unix
}:
mkDerivation {
  pname = "savage";
  version = "1.0.3";
  src = fetchgit {
    url = "https://github.com/chessai/savage.git";
    sha256 = "0lcgbjd71m38j7bp268w01f9rqk4qv57hw2ij40r9688xbbrglkw";
    rev = "2e6a7298854224a30c83515160663f2e6104a611";
  };
  libraryHaskellDepends = [
    base bytestring containers exceptions mmorph monad-control mtl
    primitive random resourcet text time transformers transformers-base
    unix
  ];
  homepage = "https://github.com/chessai/savage";
  description = "re-export of the random generators from Hedgehog";
  license = stdenv.lib.licenses.bsd3;
}
