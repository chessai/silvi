{ mkDerivation, base, bytestring, containers, exceptions, fetchgit
, mmorph, monad-control, mtl, primitive, random, resourcet, stdenv
, text, time, transformers, transformers-base, unix
}:
mkDerivation {
  pname = "savage";
  version = "1.0.2";
  src = fetchgit {
    url = "https://github.com/chessai/savage.git";
    sha256 = "0ljx9pi6jwbk310zwd6a730sxgniv6w36sqi26lrqhbjqv8md3b5";
    rev = "125b6f99cc1a543a1116c6621c459851aa0de00f";
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
