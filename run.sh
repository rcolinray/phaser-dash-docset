#!/bin/bash
cabal run
tar --exclude='.DS_Store' -cvzf Phaser.tgz Phaser.docset
