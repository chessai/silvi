{ package ? "silvi", compiler ? "ghc822" }:

(import ./default.nix {
  inherit package compiler;
}).silvi
