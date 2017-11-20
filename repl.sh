#!/usr/bin/env bash

# run stylish-haskell
#stylish-haskell -c stylish-haskell.yaml ##-i $(find . -name \*.hs)

# dist can cause problems with nix-build (called by nix-shell)
# so delete this in between builds
rm -rf dist

#the following are workarounds while cabal-repl is broken (https://github.com/haskell/cabal/issues/4602)

## library
#nix-shell -A silvi.env release.nix --run "cabal update; runhaskell Setup.hs configure; runhaskell Setup.hs repl lib:silvi"
## executable
nix-shell -A silvi.env release.nix --run "runhaskell Setup.hs configure; runhaskell Setup.hs repl exe:silvi"
# dist can cause problems with nix-build (called by nix-shell)
# so delete this in between builds
rm -rf dist
