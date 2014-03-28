{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Shelly

import Data.Text.Lazy as LT
default (LT.Text)

-- Create the Docset Folder
-- Copy existing HTML Documentation
-- Create the Info.plist File
-- Create the SQLite index
-- Populate the SQLite index

main :: IO ()
main = shelly $ verbosely $ do
  mkdir_p "Phaser.docset/Contents/Resources/Documents/"