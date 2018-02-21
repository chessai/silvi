{ mkDerivation, aeson, base, fetchgit, prim-array, primitive
, QuickCheck, stdenv, transformers, vector
}:
mkDerivation {
  pname = "quickcheck-classes";
  version = "0.3.2";
  src = fetchgit {
    url = "https://github.com/chessai/quickcheck-classes.git";
    sha256 = "0aw9cxia12k2vbxn3ssgk02amk9l5wq5494yp01i4d22gf734048";
    rev = "741190ca361aaf5c853cb7a7ddb82e12eb14bc14";
  };
  libraryHaskellDepends = [
    aeson base prim-array primitive QuickCheck transformers
  ];
  testHaskellDepends = [ aeson base primitive QuickCheck vector ];
  homepage = "https://github.com/andrewthad/quickcheck-classes#readme";
  description = "QuickCheck common typeclasses";
  license = stdenv.lib.licenses.bsd3;
}
