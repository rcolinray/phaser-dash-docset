phaser-dash-docset
==================

A Dash Docset for Phaser.

### Get the Docset

Instructions on how to download the Docset can be found [here](http://rcolinray.github.io/phaser-dash-docset).

### Build the Docset

After downloading the repository, the following commands will build the docset.

```
git submodule init
git submodule update
cabal sandbox init
cabal install --only-dependencies
cabal build
cabal run
```

### Features

The current release can index all classes, properties, and methods in the Phaser namespace.
