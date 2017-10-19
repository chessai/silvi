{ mkDerivation, attoparsec, base, chronos, ip, quickcheck, stdenv
, text
}:
mkDerivation {
  pname = "silvi";
  version = "1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    attoparsec base chronos ip quickcheck text
  ];
  homepage = "https://github.com/chessai/silvi";
  description = "A generator for different kinds of logs";
  license = stdenv.lib.licenses.asl20;
}
