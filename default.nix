{ package ? "silvi", compiler ? "ghc822" }:

let
  fetchNixpkgs = import ./nix/fetchNixpkgs.nix;
  nixpkgs = fetchNixpkgs {
    rev = "1317428ca026ec1d438e68cb0308b38028777492"; 
    sha256 = "0r9rxk2z15vf3mr0kqwls0p8fh3vvqzg677rzkcxdhnznhjkrnl0"; 
  };
  pkgs = import nixpkgs { config = {}; overlays = []; };
  inherit (pkgs) haskell;

  filterPredicate = p: type:
    let path = baseNameOf p; in !(
         (type == "directory" && path == "dist")
      || (type == "symlink"   && path == "result")
      || (type == "directory" && path == ".git")
      || (type == "symlink"   && pkgs.lib.hasPrefix "result" path)
      || pkgs.lib.hasSuffix "~" path
      || pkgs.lib.hasSuffix ".o" path
      || pkgs.lib.hasSuffix ".so" path
      || pkgs.lib.hasSuffix ".nix" path);
   
  overrides = haskell.packages.${compiler}.override {
    overrides = self: super:
    with haskell.lib;
    with { cp = file: (self.callPackage (./nix/haskell + "/${file}.nix") {});
           build = name: path: self.callCabal2nix name (builtins.filterSource filterPredicate path) {};
         };        
    {
      #vector     = cp "vector";
      # appendConfigureFlag (cp "vector") "-f unsafechecks"; 
      #http-types = cp "http-types";
      #chronos    = cp "chronos";
      savage     = cp "savage"; 
      #ip         = cp "ip"; 
      #quickcheck-classes = cp "quickcheck-classes";

      silvi      = overrideCabal (build "silvi" ./.) (drv: {
        doCheck = true;
        doHaddock = false;
      });
      
    };
  };
in rec {
  drv = overrides.${package};
  silvi = if pkgs.lib.inNixShell then drv.env else drv;
}
