#!/bin/bash
git submodule init
git submodule update
cabal sandbox init
cabal install --only-dependencies
cabal configure
cabal build
