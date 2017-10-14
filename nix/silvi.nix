{ mkDerivation, base, ip, random-fu, stdenv }:
mkDerivation {
  pname = "silvi";
  version = "1.0.0";
  src = ./..;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base ip random-fu ];
  executableHaskellDepends = [ base ip random-fu ];
  homepage = "https://github.com/chessai/silvi";
  description = "A generator for different kinds of logs";
  license = stdenv.lib.licenses.asl20;
}
