{ package ? "silvi", compiler ? "ghc822" }:

let
  fetchNixpkgs = import ./nix/fetchNixpkgs.nix;
  nixpkgs = fetchNixpkgs {
    rev = "e5629dc51a313c3b99725616718d2deff49cd891"; 
    sha256 = "0s3a65mswqds5395cqy9d4mj8d75vii2479y4dvyagamv1zh0zp6"; 
  };
  pkgs = import nixpkgs { config = {}; };
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
      mkDerivation = args: super.mkDerivation (args // {
        doCheck = pkgs.lib.elem args.pname [ ];
        doHaddock = false; 
      });
      vector     = cp "vector";
      # appendConfigureFlag (cp "vector") "-f unsafechecks"; 
      # http-types = cp "http-types";
      # chronos    = cp "chronos";
      # savage     = cp "savage"; 
      # ip         = cp "ip"; 
      silvi      = build "silvi" ./.;
      
    };
  };
in rec {
  drv = overrides.${package};
  silvi = if pkgs.lib.inNixShell then drv.env else drv;
}
