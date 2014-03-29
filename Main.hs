{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Shelly (shelly, verbosely, mkdir_p, cp_r, rm_rf, cp, test_d, echo)
import qualified Data.Text.Lazy as LT
import Database.HDBC
import Database.HDBC.Sqlite3
import System.Directory (getDirectoryContents)
import System.FilePath (dropExtension, takeExtensions)
import Text.HTML.TagSoup

default (LT.Text)

initDocset :: IO ()
initDocset = shelly $ do
  -- Create the Docset Folder
  echo "Creating Docset directory structure"
  mkdir_p "Phaser.docset/Contents/Resources/"

  -- Copy existing HTML Documentation
  exists <- test_d "Phaser.docset/Contents/Resources/Documents" 
  if not exists
    then do
      echo "Creating Docset documents"
      cp_r "phaser/docs" "Phaser.docset/Contents/Resources/Documents"
    else echo "Docset documents already exist; ignoring"

  echo "Cleaning up unnecessary files"
  rm_rf "Phaser.docset/Contents/Resources/Documents/build"

  -- Create the Info.plist File
  echo "Creating Info.plist"
  cp "Info.plist" "Phaser.docset/Contents/"

  echo "Docset initialized"

addDBEntry conn name token url = run conn ("INSERT OR IGNORE INTO searchIndex(name, type, path) VALUES ('" ++ name ++ "', '" ++ token ++ "', '" ++ url ++ "');") []

addNamespaceEntry :: Connection -> FilePath -> IO Integer
addNamespaceEntry conn file = 
  addDBEntry conn name "Namespace" file
  where name = dropExtension file

addClassEntry :: Connection -> FilePath -> IO Integer
addClassEntry conn file = 
  addDBEntry conn name "Class" file
  where name = dropExtension file

addMemberEntry :: Connection -> FilePath -> String -> IO Integer
addMemberEntry conn file member =
  addDBEntry conn name "Property" url
  where name = (dropExtension file) ++ "." ++ member
        url = file ++ "#" ++ member

addMethodEntry :: Connection -> FilePath -> String -> IO Integer
addMethodEntry conn file member =
  addDBEntry conn name "Method" url
  where name = (dropExtension file) ++ "." ++ member
        url = file ++ "#" ++ member

initDatabase :: (Connection -> IO ()) -> IO ()
initDatabase populate = do
  -- Create the SQLite index
  conn <- connectSqlite3 "Phaser.docset/Contents/Resources/docSet.dsidx"
  handleSql (\_ -> return 0) $ run conn "DROP TABLE searchIndex;" []
  run conn "CREATE TABLE searchIndex(id INTEGER PRIMARY KEY, name TEXT, type TEXT, path TEXT);" []
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
  addNamespaceEntry conn "Phaser.html"

isMembersHeader tag = tag ~== TagText "Members"
isMethodsHeader tag = tag ~== TagText "Methods"
isDocTag tag = tag ~== TagOpen "h4" [("class", "name")]

members tags = map (fromAttrib "id") $ filter isDocTag $ takeWhile (not . isMethodsHeader) $ dropWhile (not . isMembersHeader) tags
methods tags = map (fromAttrib "id") $ filter isDocTag $ dropWhile (not . isMethodsHeader) tags

populateClass :: Connection -> FilePath -> IO ()
populateClass conn file = do
  addClassEntry conn file
  src <- readFile $ "phaser/docs/" ++ file
  let tags = parseTags src
  let memberNames = members tags
  mapM_ (addMemberEntry conn file) memberNames
  let methodNames = methods tags
  mapM_ (addMethodEntry conn file) methodNames

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
