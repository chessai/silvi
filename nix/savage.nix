{ mkDerivation, base, bytestring, containers, exceptions, fetchgit
, mmorph, monad-control, mtl, primitive, random, resourcet, stdenv
, text, time, transformers, transformers-base, unix
}:
mkDerivation {
  pname = "savage";
  version = "1.0.2";
  src = fetchgit {
    url = "https://github.com/chessai/savage.git";
    sha256 = "0l5q4srkrn8angh22px03x2wdjf566qi24zh1z3rbi45qxaqp7lr";
    rev = "a4aa83b79f49f76410f5f7d8d35ac8fe60826d1b";
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
