{-# LANGUAGE OverloadedStrings #-}

module Config
    ( CColors (..)
    , CKeys (..)
    , parseConfig
    )
    where

import Prelude hiding (readFile)
import Graphics.Vty.Attributes.Color
import Graphics.Vty.Input.Events

import Data.Char (toUpper)
import Data.Ini.Config
import Data.List (find)
import Data.Maybe
import Data.Text.IO (readFile)

{-------------------------------------------------------------------------------------------------
                                            DataTypes
-------------------------------------------------------------------------------------------------}
data CColors = CColors
    { itemBG        :: Color
    , itemFG        :: Color
    , selFG         :: Color
    , selBG         :: Color
    , fileBG        :: Color
    , fileFG        :: Color
    , editBordBG    :: Color
    , editBordFG    :: Color
    , editLableFG   :: Color
    , editLableBG   :: Color
    , editBG        :: Color
    , editFG        :: Color
    } deriving Show

data CKeys = CKeys
    { insert    :: Event
    , top       :: Event
    , append    :: Event
    , bottom    :: Event
    , editMode  :: Event
    , delete    :: Event
    , check     :: Event
    , up        :: Event
    , down      :: Event
    , moveUp    :: Event
    , moveDown  :: Event
    , quit      :: Event
    } deriving Show

data MKeys = MKeys
    { insertm    :: Maybe Event
    , topm       :: Maybe Event
    , appendm    :: Maybe Event
    , bottomm    :: Maybe Event
    , editm      :: Maybe Event
    , deletem    :: Maybe Event
    , checkm     :: Maybe Event
    , upm        :: Maybe Event
    , downm      :: Maybe Event
    , moveUpm    :: Maybe Event
    , moveDownm  :: Maybe Event
    , quitm      :: Maybe Event
    } deriving Show

data MColors = MColors
    { itemBGm        :: Maybe Color
    , itemFGm        :: Maybe Color
    , selBGm         :: Maybe Color
    , selFGm         :: Maybe Color
    , fileBGm        :: Maybe Color
    , fileFGm        :: Maybe Color
    , editBordBGm    :: Maybe Color
    , editBordFGm    :: Maybe Color
    , editLableBGm   :: Maybe Color
    , editLableFGm   :: Maybe Color
    , editBGm        :: Maybe Color
    , editFGm        :: Maybe Color
    } deriving Show


{-------------------------------------------------------------------------------------------------
                                            Default Values
-------------------------------------------------------------------------------------------------}
defaultColors :: CColors
defaultColors = CColors
    { itemBG        = black
    , itemFG        = brightWhite
    , selBG         = brightCyan
    , selFG         = black
    , fileBG        = cyan
    , fileFG        = black
    , editBordBG    = black
    , editBordFG    = yellow
    , editLableBG   = black
    , editLableFG   = cyan
    , editBG        = black
    , editFG        = brightWhite
    }

defaultKeys :: CKeys
defaultKeys = CKeys
    { insert    = (EvKey (KChar 'i') []) 
    , top       = (EvKey (KChar 'I') [])
    , append    = (EvKey (KChar 'a') [])
    , bottom    = (EvKey (KChar 'A') [])
    , editMode  = (EvKey (KChar 'e') [])
    , delete    = (EvKey (KChar 'd') [])
    , check     = (EvKey (KChar 'c') [])
    , up        = (EvKey (KChar 'j') [])
    , down      = (EvKey (KChar 'k') [])
    , moveUp    = (EvKey (KChar 'J') [])
    , moveDown  = (EvKey (KChar 'K') [])
    , quit      = (EvKey (KChar 'q') [])
    }

{-------------------------------------------------------------------------------------------------
                                            Parse File
-------------------------------------------------------------------------------------------------}
parseConfig :: Maybe FilePath -> IO (CColors, CKeys)
parseConfig fpath = case fpath of
    Just path -> do
        content <- readFile path
        case parseIniFile content configParser of
            Left _ -> return (defaultColors, defaultKeys)
            Right (mc, mk) -> return ( createColors mc, createKeys mk)
    Nothing -> return (defaultColors, defaultKeys)

configParser :: IniParser (Maybe MColors, Maybe MKeys)
configParser = do
    -- parse colors
    color <- sectionMb "COLOR" $ do
        mItemBG        <- fieldMbOf "itemBG" string
        mItemFG        <- fieldMbOf "itemFG" string
        mSelBG         <- fieldMbOf "selectionBG" string
        mSelFG         <- fieldMbOf "selectionFG" string
        mFileBG        <- fieldMbOf "fileBG" string
        mFileFG        <- fieldMbOf "fileFG" string
        mEditBordBG    <- fieldMbOf "editorBorderBG" string
        mEditBordFG    <- fieldMbOf "editorBorderFG" string
        mEditLableBG   <- fieldMbOf "editoLableBG" string
        mEditLableFG   <- fieldMbOf "editoLableFG" string
        mEditBG        <- fieldMbOf "editorBG" string
        mEditFG        <- fieldMbOf "editorFG" string
        -- return maybe variation of struct
        return MColors
            { itemBGm        = mItemBG      >>= parseColor
            , itemFGm        = mItemFG      >>= parseColor
            , selBGm         = mSelBG       >>= parseColor
            , selFGm         = mSelFG       >>= parseColor
            , fileBGm        = mFileBG      >>= parseColor
            , fileFGm        = mFileFG      >>= parseColor
            , editBordBGm    = mEditBordBG  >>= parseColor
            , editBordFGm    = mEditBordFG  >>= parseColor
            , editLableBGm   = mEditLableBG >>= parseColor
            , editLableFGm   = mEditLableFG >>= parseColor
            , editBGm        = mEditBG      >>= parseColor
            , editFGm        = mEditFG      >>= parseColor
            }
    -- parse keybinds
    keys <- sectionMb "KEYBINDS" $ do
        mInsert    <- fieldMbOf "insert" string
        mTop       <- fieldMbOf "insertTop" string
        mAppend    <- fieldMbOf "append" string
        mBottom    <- fieldMbOf "insertBottom" string
        mEdit      <- fieldMbOf "edit" string
        mDelete    <- fieldMbOf "delete" string
        mCheck     <- fieldMbOf "toggleCheck" string
        mUp        <- fieldMbOf "up" string
        mDown      <- fieldMbOf "down" string
        mMoveUp    <- fieldMbOf "moveUp" string
        mMoveDown  <- fieldMbOf "moveDown" string
        mQuit      <- fieldMbOf "quit" string
        -- return maybe variation of keys
        return MKeys
            { insertm    = mInsert      >>= parseKey  
            , topm       = mTop         >>= parseKey     
            , appendm    = mAppend      >>= parseKey  
            , bottomm    = mBottom      >>= parseKey  
            , editm      = mEdit        >>= parseKey    
            , deletem    = mDelete      >>= parseKey  
            , checkm     = mCheck       >>= parseKey  
            , upm        = mUp          >>= parseKey 
            , downm      = mDown        >>= parseKey   
            , moveUpm    = mMoveUp      >>= parseKey 
            , moveDownm  = mMoveDown    >>= parseKey   
            , quitm      = mQuit        >>= parseKey   
            }
    -- return final product
    return (color, keys)
{-------------------------------------------------------------------------------------------------
                                            Merge Config with defaults
-------------------------------------------------------------------------------------------------}
createColors :: Maybe MColors -> CColors
createColors p = case p of
    Just m -> CColors
        { itemBG        = fromMaybe black $ itemBGm m
        , itemFG        = fromMaybe brightWhite $ itemFGm m
        , selBG         = fromMaybe brightCyan $ selBGm m
        , selFG         = fromMaybe black $ selFGm m
        , fileBG        = fromMaybe cyan $ fileBGm m
        , fileFG        = fromMaybe black $ fileFGm m
        , editBordBG    = fromMaybe black $ editBordBGm m
        , editBordFG    = fromMaybe yellow $ editBordFGm m
        , editLableBG   = fromMaybe black $ editLableBGm m
        , editLableFG   = fromMaybe cyan $ editLableFGm m
        , editBG        = fromMaybe black $ editBGm m
        , editFG        = fromMaybe brightWhite $ editFGm m
        }
    Nothing -> defaultColors

createKeys :: Maybe MKeys -> CKeys
createKeys p = case p of
    Just m -> CKeys
        { insert    = fromMaybe (EvKey (KChar 'i') []) $ insertm m 
        , top       = fromMaybe (EvKey (KChar 'I') []) $ topm m
        , append    = fromMaybe (EvKey (KChar 'a') []) $ appendm m
        , bottom    = fromMaybe (EvKey (KChar 'A') []) $ bottomm m
        , editMode  = fromMaybe (EvKey (KChar 'e') []) $ editm m
        , delete    = fromMaybe (EvKey (KChar 'd') []) $ deletem m
        , check     = fromMaybe (EvKey (KChar 'c') []) $ checkm m
        , up        = fromMaybe (EvKey (KChar 'j') []) $ upm m
        , down      = fromMaybe (EvKey (KChar 'k') []) $ downm m
        , moveUp    = fromMaybe (EvKey (KChar 'J') []) $ moveUpm m
        , moveDown  = fromMaybe (EvKey (KChar 'K') []) $ moveDownm m
        , quit      = fromMaybe (EvKey (KChar 'q') []) $ quitm m
        }
    Nothing -> defaultKeys

{-------------------------------------------------------------------------------------------------
                                            Parse String to Config-Data
-------------------------------------------------------------------------------------------------}
-- | parse string input to color (RGB/ISO)
parseColor :: String -> Maybe Color
parseColor ('#' : xs) = if length xs /= 6
    then Nothing
    else do
        r <- fromHex $ take 2 xs
        g <- fromHex $ take 2 . drop 2 $ xs
        b <- fromHex $ take 2 . drop 4 $ xs
        return $ rgbColor r g b
parseColor s = isoColor s

-- | parse string into EvKey (Key + Modifier)
parseKey :: String -> Maybe Event
parseKey s = case xs of
        Just splitted ->
            let ckey = stringToKey $ head splitted
                mods = catMaybes $ map modFromStr $ tail splitted
            in if length mods == length (tail splitted)
                then case ckey of
                    Just key -> let (k, m) = (applyShift key mods)
                        in Just $ EvKey k m
                    Nothing -> Nothing
                else Nothing
            -- key >>= return $ EvKey key mods
        Nothing -> Nothing
    where
        xs = if split s == []
            then Nothing
            else Just $ reverse $ split s

{-------------------------------------------------------------------------------------------------
                                            Util - Color
-------------------------------------------------------------------------------------------------}
-- convert valid hex string to int
fromHex :: String -> Maybe Int
fromHex [] = Just 0
fromHex s = do
    val <- hexVal $ last s
    rest <- fromHex $ init s
    return (val + 16 * rest)

-- get decimal value from hex val
hexVal ::Char -> Maybe Int
hexVal '0' = Just 0
hexVal '1' = Just 1
hexVal '2' = Just 2
hexVal '3' = Just 3
hexVal '4' = Just 4
hexVal '5' = Just 5
hexVal '6' = Just 6
hexVal '7' = Just 7
hexVal '8' = Just 8
hexVal '9' = Just 9
hexVal 'A' = Just 10
hexVal 'B' = Just 11
hexVal 'C' = Just 12
hexVal 'D' = Just 13
hexVal 'E' = Just 14
hexVal 'F' = Just 15
hexVal 'a' = Just 10
hexVal 'b' = Just 11
hexVal 'c' = Just 12
hexVal 'd' = Just 13
hexVal 'e' = Just 14
hexVal 'f' = Just 15
hexVal _ = Nothing

-- get ISO Color from String
isoColor :: String -> Maybe Color
isoColor s = case s of
    "black"         -> Just black
    "red"           -> Just red
    "green"         -> Just green
    "yellow"        -> Just yellow
    "blue"          -> Just blue
    "magenta"       -> Just magenta
    "cyan"          -> Just cyan
    "white"         -> Just white
    "brightBlack"   -> Just brightBlack
    "brightRed"     -> Just brightRed
    "brightGreen"   -> Just brightGreen
    "brightYellow"  -> Just brightYellow
    "brightBlue"    -> Just brightBlue
    "brightMagenta" -> Just brightMagenta
    "brightCyan"    -> Just brightCyan
    "brightWhite"   -> Just brightWhite
    _               -> Nothing


{-------------------------------------------------------------------------------------------------
                                            Util - Keys
-------------------------------------------------------------------------------------------------}
-- split string on ' ' and '+'
split :: String -> [String]
split = splitOn (\c -> c == ' ' || c == '+')

-- split string on specific condition
splitOn :: (Char -> Bool) -> String -> [String]
splitOn f s = case dropWhile f s of
    [] -> []
    xs -> w : splitOn f ys
        where (w, ys) = break f xs

-- get Modifier from string
modFromStr :: String -> Maybe Modifier
modFromStr "alt"     = Just MAlt
modFromStr "control" = Just MCtrl
modFromStr "shift"   = Just MShift
modFromStr "win"     = Just MMeta
modFromStr _         = Nothing

-- check if its a special key
stringToKey :: String -> Maybe Key
stringToKey "backspace"    = Just KBS
stringToKey "delete"       = Just KDel
stringToKey "enter"        = Just KEnter
stringToKey "left"         = Just KLeft
stringToKey "right"        = Just KRight
stringToKey "up"           = Just KUp
stringToKey "down"         = Just KDown
stringToKey "plus"         = Just $ KChar '+'
stringToKey "minus"        = Just $ KChar '-'
stringToKey s              = if length s == 1
    then Just $ KChar $ head s
    else Nothing

applyShift :: Key -> [Modifier] -> (Key, [Modifier])
applyShift (KChar c) m = case find (== MShift) m of
    Just _  -> (KChar $ toUpper c, [x | x <- m, x /= MShift])
    Nothing -> (KChar c, m)
applyShift k m = (k, m) 