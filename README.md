phaser-dash-docset
==================

A Dash Docset for Phaser.


### Features

The current release can index all classes, properties, and methods in the Phaser namespace.


### Get the Docset

The Phaser docset is now hosted through the Dash user contributions. You can download it in Phaser by going to Preferences > Downloads > User Contributed.

### Build the Docset

After downloading the repository, the following commands will build the docset.

```
git submodule init
git submodule update
cabal sandbox init
cabal install --only-dependencies
cabal configure
cabal build
cabal run
```
