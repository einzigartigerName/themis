{-# LANGUAGE ViewPatterns #-}

module FileIO
    ( TodoLocation (..)
    , getFilePath
    , readItemsFromFile
    , stdFileName
    , writeItemsToFile
    )
    where

import Task

import Control.Exception
import System.Directory
import System.IO
import Text.Printf
import Data.List
import Data.Char
import Data.Maybe (catMaybes)

data TodoLocation = CurrentDir | StdDir

-- standardd file name to be used as input/output file
stdFileName :: FilePath
stdFileName = "todo.md"

-- look for file in current directory, then homefolder
getFilePath :: TodoLocation -> IO FilePath
getFilePath CurrentDir = do
    return stdFileName
getFilePath StdDir = do
    home <- getHomeDirectory
    let path = home ++ "/" ++ stdFileName
    return path

-- | Writes all tasks contained in Tasks into the file located at the given FilePath 
writeItemsToFile :: FilePath -> Tasks -> IO ()
writeItemsToFile path tasks = writeFile path (serializeF tasks)

-- | Reads all Items from the file located at the given FilePath and return them
readItemsFromFile :: FilePath -> IO [Item]
readItemsFromFile path = do
    exists <- doesFileExist path
    if not exists
        -- open file and close without writing
        then do
            result <- try $ openFile path WriteMode :: IO (Either IOException Handle)
            case result of
                -- unable to open
                Left _ -> do
                    hPrintf stderr "Unable to create file at %s\n" path
                    return []
                -- close without writing
                Right fh -> do
                    hClose fh
                    return []
        else do
            -- lazy read all lines in list
            ls <- fmap lines $ readFile path
            -- parse lines and create list of Maybe Item
            let (_, mes) = foldl (\(nID, acc) x -> let (outID, xe) = processLine x nID in
                    (outID, xe : acc)) (1, []) ls
            -- return only [Item] in [Maybe Item] (filter Nothing)
            return (catMaybes mes)


-- return tupel of next ID for the next Item to be passed and current result of parsing
processLine :: String -> ID -> (ID, Maybe Item)
processLine line nid = case parse line nid of
    Just _ -> (nid + 1, parse line nid)
    Nothing -> (nid, Nothing)

-- parse line  to Item and applies the given ID
parse :: String -> ID -> Maybe Item
parse (x : xs) nid
    | isSpace x = parse xs nid
    | isOpeningBrack x = parseDone (x : xs) nid
parse _ _ = Nothing

-- check if task marked as done
parseDone :: String -> ID -> Maybe Item
parseDone (stripPrefix "[ ]" -> Just rest) eid = constructItem False eid (stripLeadingWhitespace rest)
parseDone (stripPrefix "[x]" -> Just rest) eid = constructItem True eid (stripLeadingWhitespace rest)
parseDone _ _ = Nothing

-- constrcut Item from given Checked value, ID and String
constructItem :: Bool -> ID -> String -> Maybe Item
constructItem _ _ [] = Nothing
constructItem b eid t = Just(Item {iID = eid, checked = b, text = t})




{-----------------Utility------------------}
stripLeadingWhitespace :: String -> String
stripLeadingWhitespace (' ' : xs) = stripLeadingWhitespace xs
stripLeadingWhitespace xs = xs 

isOpeningBrack :: Char -> Bool
isOpeningBrack c = c == '['

-- isClosingBrack :: Char -> Bool
-- isClosingBrack c = c == ']'
-- 
-- isHeading :: Char -> Bool
-- isHeading c = c == '#'
-- 
-- isListItem :: Char -> Bool
-- isListItem c = c == '*'