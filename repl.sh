#!/usr/bin/env bash

#this is a workaround while cabal-repl is broken (https://github.com/haskell/cabal/issues/4602)
runhaskell Setup.hs configure
runhaskell Setup.hs repl exe:silvi
