{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Shelly (shelly, verbosely, mkdir_p, cp_r, rm_rf, cp)
import qualified Data.Text.Lazy as LT
import Database.HDBC
import Database.HDBC.Sqlite3
import System.Directory (getDirectoryContents)
import System.FilePath (takeExtensions)

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
  cp "Nodes.xml" "Phaser.docset/Contents/Resources/"

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

isDocFile :: FilePath -> Bool
isDocFile "." = False
isDocFile ".." = False
isDocFile "build" = False
isDocFile "img" = False
isDocFile "scripts" = False
isDocFile "styles" = False
isDocFile "index.html" = False
isDocFile "classes.list.html" = False
isDocFile "namespaces.list.html" = False
isDocFile "Phaser.html" = False
isDocFile file
  | ext == ".js.html" = False
  | ext == ".js_.html" = False
  | ext == ".js__.html" = False
  | ext == ".js___.html" = False
  | otherwise = True
  where ext = takeExtensions file

populateDatabase :: Connection -> IO ()
populateDatabase conn = do
  contents <- getDirectoryContents "phaser/docs"
  let docFiles = filter isDocFile contents
  run conn "INSERT OR IGNORE INTO searchIndex(name, type, path) VALUES ('Phaser', 'Namespace', 'Phaser.html');" []
  mapM_ putStrLn docFiles

main :: IO ()
main = do
  initDocset
  initDatabase populateDatabase