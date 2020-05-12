{-# LANGUAGE OverloadedStrings #-}
module Main where

import Tui
import FileIO

import System.Directory
import System.Environment


parseArgs :: [String] -> IO ()
-- no args -> start by selecting file to use
parseArgs [] = do
    flag <- (do
        local <- doesFileExist stdFileName
        if local
            then return CurrentDir
            else return StdDir)
    execute $ Left flag
-- force use of std dir
parseArgs ["-d"] = execute $ Left StdDir
-- force use of current dir
parseArgs ["-c"] = execute $ Left CurrentDir 
-- force use of given file
parseArgs ("-f" : f) = case f of
    [] -> putStrLn "No File provided!"
    (x : _) -> execute $ Right x
-- unknown args
parseArgs _ = putStrLn "Unknown Arguments!"

-- start with file as base
execute :: Either TodoLocation FilePath -> IO ()
execute e = do
    fpath <- case e of
        Left l -> getFilePath l
        Right f -> return f
    items <- readItemsFromFile fpath
    tui (reverse items) fpath

-- main
main :: IO ()
main = do
    args <- getArgs
    parseArgs args