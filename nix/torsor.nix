{ mkDerivation, base, fetchgit, stdenv }:
mkDerivation {
  pname = "torsor";
  version = "0.1";
  src = fetchgit {
    url = "https://github.com/andrewthad/torsor.git";
    sha256 = "1lpj1nvifk5hs25805fwg8d0biy5f6dkcdc83bdn64xppczaik28";
    rev = "4c5194f8a5e4665c7a58ebc7b0bf79596070c35b";
  };
  libraryHaskellDepends = [ base ];
  homepage = "https://github.com/andrewthad/torsor#readme";
  description = "Torsor Typeclass";
  license = stdenv.lib.licenses.bsd3;
}
