{ mkDerivation, ansi-terminal, async, base, bytestring
, concurrent-output, containers, directory, exceptions, fetchgit
, lifted-async, mmorph, monad-control, mtl, pretty-show, primitive
, random, resourcet, stdenv, stm, template-haskell, text, th-lift
, time, transformers, transformers-base, unix, wl-pprint-annotated
}:
mkDerivation {
  pname = "savage";
  version = "1.0.1";
  src = fetchgit {
    url = "https://github.com/chessai/savage.git";
    sha256 = "1k0z0sxnhl30pjc23ip717fpwhxaj2rpwww8rm93vqa6gy4nx3kb";
    rev = "15c131568417e70405b8aa0658bd68ab05205ed8";
  };
  libraryHaskellDepends = [
    ansi-terminal async base bytestring concurrent-output containers
    directory exceptions lifted-async mmorph monad-control mtl
    pretty-show primitive random resourcet stm template-haskell text
    th-lift time transformers transformers-base unix
    wl-pprint-annotated
  ];
  homepage = "https://github.com/chessai/savage";
  description = "re-export of the random generators from Hedgehog";
  license = stdenv.lib.licenses.bsd3;
}
