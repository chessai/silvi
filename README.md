# `silvi`

[![Hackage][hackage-badge]][hackage-link]
[![Stackage][stackage-badge]][stackage-link]
[![License][license-badge]][license-link]

`silvi` is a Haskell library for generating fake logs from user specifications.

## Using as a library

### For Nix Users
```sh
nix-env -iA nixos.haskellPackages.silvi
```
then import as one normally would with ```hs import Silvi```.

### For non-Nix Users
```
cabal install silvi
```
then import as one normally would with ```hs import Silvi```.
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

[hackage-badge]:
    https://img.shields.io/hackage/v/silvi.svg?label=Hackage
[hackage-link]:
    https://hackage.haskell.org/package/silvi
[stackage-badge]:
    https://www.stackage.org/package/silvi/badge/lts?label=Stackage
[stackage-link]:
    https://www.stackage.org/package/silvi
[license-badge]:
    https://img.shields.io/badge/License-Apache%202.0-blue.svg
[license-link]:
    https://spdx.org/licenses/Apache-2.0.html
