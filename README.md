# `silvi`

[![Hackage][hackage-badge]][hackage-link]
[![Stackage][stackage-badge]][stackage-link]
[![License][license-badge][license-link]

`silvi` is a Haskell library for generating fake logs from user specifications.

## Using `silvi` as a library

### For Nix Users
```sh
nix-env -iA nixos.haskellPackages.silvi
```

### For non-Nix Users
```
cabal install silvi
```

## Running as an executable

### For Nix Users
```sh
./run.sh
```
Note: This will run `silvi` in a cabal repl inside of a nix-shell.

### For non-Nix Users
```sh
./repl.sh
```
Note: This will run `silvi` in a cabal repl.
