#!/usr/bin/env bash

# run stylish-haskell
#stylish-haskell -c stylish-haskell.yaml ##-i $(find . -name \*.hs)

# dist can cause problems with nix-build (called by nix-shell)
# so delete this in between builds
rm -rf dist

nix-shell default.nix -A silvi --run "cabal repl"

# dist can cause problems with nix-build (called by nix-shell)
# so delete this in between builds
rm -rf dist
