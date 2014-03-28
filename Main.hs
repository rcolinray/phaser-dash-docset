{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Shelly (shelly, verbosely, mkdir_p, cp_r, rm_rf, cp)
import qualified Data.Text.Lazy as LT
import Database.HDBC
import Database.HDBC.Sqlite3
import System.Directory (getDirectoryContents)

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

initDatabase :: (Connection -> IO ()) -> IO ()
initDatabase populate = do
  -- Create the SQLite index
  conn <- connectSqlite3 "Phaser.docset/Contents/Resources/docSet.dsidx"
  handleSql (\_ -> return 0) $ run conn "DROP TABLE searchIndex;" []
  run conn "CREATE TABLE searchIndex(id INTEGER PRIMARY KEY, name TEXT, type TEXT, path TEXT);" []
  run conn "CREATE UNIQUE INDEX anchor ON searchIndex (name, type, path);" []

  populate conn

  commit conn
  disconnect conn

populateDatabase :: Connection -> IO ()
populateDatabase conn = putStrLn "Hello, world!"

main :: IO ()
main = do
  initDocset
  initDatabase populateDatabase