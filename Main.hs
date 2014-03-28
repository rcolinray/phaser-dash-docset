{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Shelly (shelly, verbosely, mkdir_p, cp_r, rm_rf, cp)
import qualified Data.Text.Lazy as LT
import Database.HDBC
import Database.HDBC.Sqlite3
import System.Directory (getDirectoryContents)
import System.FilePath (dropExtension, takeExtensions)

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

addDBEntry :: Connection -> String -> String -> IO Integer
addDBEntry conn file token = 
  let name = dropExtension file
  in run conn ("INSERT OR IGNORE INTO searchIndex(name, type, path) VALUES ('" ++ name ++ "', '" ++ token ++ "', '" ++ file ++ "');") []

initDatabase :: (Connection -> IO ()) -> IO ()
initDatabase populate = do
  -- Create the SQLite index
  conn <- connectSqlite3 "Phaser.docset/Contents/Resources/docSet.dsidx"
  handleSql (\_ -> return 0) $ run conn "DROP TABLE searchIndex;" []
  run conn "CREATE TABLE searchIndex(id INTEGER PRIMARY KEY, name TEXT, type TEXT, path TEXT);" []
  --run conn "CREATE UNIQUE INDEX anchor ON searchIndex (name, type, path);" []
  populate conn
  commit conn
  disconnect conn

isClassFile :: FilePath -> Bool
isClassFile "." = False
isClassFile ".." = False
isClassFile "build" = False
isClassFile "img" = False
isClassFile "scripts" = False
isClassFile "styles" = False
isClassFile "index.html" = False
isClassFile "classes.list.html" = False
isClassFile "namespaces.list.html" = False
isClassFile "Phaser.html" = False
isClassFile file
  | ext == ".js.html" = False
  | ext == ".js_.html" = False
  | ext == ".js__.html" = False
  | ext == ".js___.html" = False
  | otherwise = True
  where ext = takeExtensions file

populateNamespaces :: Connection -> IO Integer
populateNamespaces conn = do
  addDBEntry conn "Namespace" "Phaser.html"

populateClass :: Connection -> FilePath -> IO Integer
populateClass conn file = do
  addDBEntry conn file "Class"

populateClasses :: Connection -> IO ()
populateClasses conn = do
  contents <- getDirectoryContents "phaser/docs"
  let docFiles = filter isClassFile contents
  mapM_ (populateClass conn) docFiles

populateDatabase :: Connection -> IO ()
populateDatabase conn = do
  populateNamespaces conn
  populateClasses conn

main :: IO ()
main = do
  initDocset
  initDatabase populateDatabase