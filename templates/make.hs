#!/usr/bin/env stack
-- stack --resolver lts-9.1 script --package turtle --package text --package system-filepath --package extra --package directory

{-# LANGUAGE OverloadedStrings #-}

import           Data.Monoid
import           Data.Text          hiding (zip)
import           Data.Text.IO
import           Data.Tuple.Extra
import           Filesystem.Path    hiding (append)
import           Prelude            hiding (FilePath, writeFile)
import           System.Directory
import           System.Environment
import           Turtle             hiding (append)

type Location = (FilePath,FilePath)

main :: IO ()
main = do
        [exchange] <- arguments
        let files = ["Api.hs","Types.hs","Internal.hs","test/ApiSpec.hs"]
        let paths = append (exchange <> "/") <$> files
        makeDirs (unpack exchange)
        sequence_ $ sh . replaceFile exchange . both fromText <$> zip files paths

makeDirs :: String -> IO ()
makeDirs exchange = do
        createDirectory exchange
        createDirectory (exchange <> "/test")

replaceFile :: Text -> Location -> Shell ()
replaceFile name (file,path) = output path $ sed ("<newmodule>" *> return name) (input file)

