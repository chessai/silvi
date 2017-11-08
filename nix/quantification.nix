{ mkDerivation, aeson, base, containers, fetchgit, ghc-prim
, hashable, path-pieces, stdenv, text, unordered-containers, vector
}:
mkDerivation {
  pname = "quantification";
  version = "0.2";
  src = fetchgit {
    url = "https://github.com/andrewthad/quantification.git";
    sha256 = "04hg5r6y2agrgs580vg3h7whsjigbyy2w0wjnf9aibxaic141z5k";
    rev = "44ae229508e99e377d7842245fcd2f77257520fc";
  };
  libraryHaskellDepends = [
    aeson base containers ghc-prim hashable path-pieces text
    unordered-containers vector
  ];
  doCheck = false;
  homepage = "https://github.com/andrewthad/quantification#readme";
  description = "Rage against the quantification";
  license = stdenv.lib.licenses.bsd3;
}
