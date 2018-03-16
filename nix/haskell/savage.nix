{ mkDerivation, base, bytestring, containers, exceptions, fetchgit
, mmorph, monad-control, mtl, primitive, random, resourcet, stdenv
, text, time, transformers, transformers-base, unix
}:
mkDerivation {
  pname = "savage";
  version = "1.0.3";
  src = fetchgit {
    url = "https://github.com/chessai/savage.git";
    sha256 = "14j6ggmp7d24qb4h32q02sdlh3iarqdr6zqdn13zs0m25rzmhmqv";
    rev = "7eb5f637bbe3163c74910c84f058dd0f64108615";
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
