{-# LANGUAGE OverloadedStrings #-}
module Main where

import Tui
import FileIO
import Config

import System.Directory
import System.Environment

import Brick.Widgets.List as BL (listElements)
import Data.Vector as Vec (toList)


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
parseArgs ["-l"] = execute $ Left CurrentDir 
-- force use of given file
parseArgs ("-f" : f) = case f of
    (x : []) -> execute $ Right x
    _ -> usage
-- print usage
parseArgs ("-h" : _) = usage
-- unknown args
parseArgs x = do
    putStrLn $ "Unknown Arguments: " ++  show x 
    usage

-- print usage
usage :: IO ()
usage = do
    putStrLn "Usage:"
    putStrLn "\ttodo [ARGUMENT] [FILE]"
    putStrLn "\t-c\t- use \'todo.md\' in the current directory"
    putStrLn "\t-d\t- use \'todo.md\' in the home directory"
    putStrLn "\t-h\t- show this help dialog"
    putStrLn "\t-f FILE\t- use FILE as your todo list"




-- start with file as base
execute :: Either TodoLocation FilePath -> IO ()
execute e = do
    -- config
    cpath <- getConfigFilePath
    -- colors and keys to use
    (colors, keys) <- parseConfig cpath
    -- file to use
    fpath <- case e of
        Left l -> getFilePath l
        Right f -> return f
    -- items to use on startup
    items <- readItemsFromFile fpath
    -- end state
    exitState <- tui (reverse items) colors keys fpath
    let exitItems = Vec.toList $ BL.listElements $ _tasks exitState 
    -- write last changes to file
    writeItemsToFile fpath exitItems

-- main
main :: IO ()
main = do
    args <- getArgs
    parseArgs args
