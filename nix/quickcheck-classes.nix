{ mkDerivation, aeson, base, fetchgit, prim-array, primitive
, QuickCheck, stdenv
}:
mkDerivation {
  pname = "quickcheck-classes";
  version = "0.1";
  src = fetchgit {
    url = "https://github.com/andrewthad/quickcheck-classes.git";
    sha256 = "15pman16jxc64dfvapa008ic16f0xanibri872d45gfblzhmn87h";
    rev = "64b46adb028b5fd9105be671ffd4d28d9d46fe7b";
  };
  libraryHaskellDepends = [
    aeson base prim-array primitive QuickCheck
  ];
  testHaskellDepends = [ aeson base primitive QuickCheck ];
  doCheck = false;
  homepage = "https://github.com/andrewthad/quickcheck-classes#readme";
  description = "QuickCheck common typeclasses";
  license = stdenv.lib.licenses.bsd3;
}
