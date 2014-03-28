{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Shelly (shelly, verbosely, mkdir_p, cp_r, rm_rf, cp)
import qualified Data.Text.Lazy as LT
import Database.HDBC
import Database.HDBC.Sqlite3

default (LT.Text)

initDocset :: IO ()
initDocset = shelly $ verbosely $ do
  -- Create the Docset Folder
  mkdir_p "Phaser.docset/Contents/Resources/"

  -- Copy existing HTML Documentation
  cp_r "phaser/docs" "Phaser.docset/Contents/Resources/Documents"
  rm_rf "Phaser.docset/Contents/Resources/Documents/build"

  -- Create the Info.plist File
  cp "Info.plist" "Phaser.docset/Contents/"

main :: IO ()
main = do
  initDocset

  -- Create the SQLite index
  conn <- connectSqlite3 "Phaser.docset/Contents/Resources/docSet.dsidx"
  handleSql (\_ -> return 0) $ run conn "DROP TABLE searchIndex;" []
  run conn "CREATE TABLE searchIndex(id INTEGER PRIMARY KEY, name TEXT, type TEXT, path TEXT);" []
  run conn "CREATE UNIQUE INDEX anchor ON searchIndex (name, type, path);" []

  -- Populate the SQLite index
  -- INSERT OR IGNORE INTO searchIndex(name, type, path) VALUES ('name', 'type', 'path');
  -- Phaser Namespace
  -- Phaser Classes
  -- Each class will have constructors, attributes, and methods

  commit conn
  disconnect conn