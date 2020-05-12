{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Tui
    ( TuiState (..)
    , tui
    )
    where

import Task as T
import Cursor as C
import FileIO as F

import Lens.Micro.TH

import Brick.AttrMap
import Brick.Main
import Brick.Types
import Brick.Util
import Brick.Widgets.Edit as BE
import Brick.Widgets.Center as BC
import Brick.Widgets.Border as BB
import Brick.Widgets.Core

import Control.Monad.IO.Class (liftIO)

import Graphics.Vty.Input.Events
import Graphics.Vty.Attributes


{-------------------------------------------------------------------------------------------------
                                            DataTypes
-------------------------------------------------------------------------------------------------}
data InsertLocation = Top | Bottom | Above | Below
    deriving Eq

data TuiState = TuiState
    { tasks :: Cursor Item
    , file :: FilePath
    , nID :: Int
    , _insertEditor :: BE.Editor String String
    , showEditor :: Bool
    , insertLocal :: InsertLocation
    }
makeLenses ''TuiState

{-------------------------------------------------------------------------------------------------
                                            TUI
-------------------------------------------------------------------------------------------------}
-- | Main call, start Tui
tui :: Tasks -> FilePath -> IO TuiState
tui ts fp = do
    initialState <- buildInitialState ts fp
    defaultMain tuiApp initialState

-- | Build Tui App
tuiApp :: App TuiState e String
tuiApp =
    App
        { appDraw = drawTui
        , appChooseCursor = showFirstCursor
        , appHandleEvent = handleTuiEvent
        , appStartEvent = pure
        , appAttrMap = const $ attrMap mempty attrMappings
        }

titleAttr :: AttrName
titleAttr = "title"

selectAttr :: AttrName
selectAttr = "selected"

attrMappings :: [(AttrName, Attr)]
attrMappings =
    [ (selectAttr,      bg cyan)
    , (BB.borderAttr,   yellow `on` black)
    , (titleAttr,       fg cyan)
    ]

-- | draw Tui
drawTui :: TuiState -> [Widget String]
drawTui ts =
    let e = showEditor ts
        c = tasks ts
        i = case C.selected c of
            Just s ->
                [ vBox $
                  concat $
                    [ map (drawItem False) $ reverse $ C.next c 
                    , [drawItem True $ s]
                    , map (drawItem False) $ C.previous c
                    ]
                ]
            Nothing -> []
    in case i of
        -- if no items to render default render editor
        [] -> drawEditor $ _insertEditor ts
        -- if editor in use show, else only items
        _ -> (if e then drawEditor $ _insertEditor ts else []) ++ i

-- | draw editor
drawEditor :: Editor String String -> [Widget String]
drawEditor e = 
    [ BC.centerLayer $
      BB.borderWithLabel
        (BB.vBorder <+> (withAttr titleAttr $ str " New Item ") <+> BB.vBorder) $
      hLimit 40 $
      vLimit 3 $
      withAttr "editor" $ 
      BE.renderEditor (str . unlines) True e
    ]

-- | Draw Item in Todo List
drawItem :: Bool -> Item -> Widget n
drawItem s =
    (if s
        then withAttr selectAttr
        else id) .
    serializeItemW


{-------------------------------------------------------------------------------------------------
                                            State
-------------------------------------------------------------------------------------------------}
-- | Init TuiState
buildInitialState :: Tasks -> FilePath -> IO TuiState
buildInitialState ts fp =
    pure TuiState
        { tasks = C.fromList ts
        , file = fp
        , nID = T.nextID ts
        , _insertEditor = BE.editor "Insert" (Just 1) ""
        , showEditor = False
        , insertLocal = Bottom
        }

-- | writes changes to file
updateFile :: TuiState -> EventM String (Next TuiState)
updateFile s =
    let fpath = file s
        items = C.toList $ tasks s
    in do
        liftIO $ F.writeItemsToFile fpath items
        continue s


{-------------------------------------------------------------------------------------------------
                                            items
-------------------------------------------------------------------------------------------------}
-- | change selection
updateSelection :: Direction -> TuiState -> TuiState
updateSelection d s =
    let c = tasks s
        fn = if d == Down then C.selectPrev else C.selectNext
    in case fn c of
        Just p -> s {tasks = p}
        Nothing -> s

-- | move selection
moveSelection :: Direction -> TuiState -> TuiState
moveSelection d s =
    let c = tasks s
        fn = if d == Down then C.moveSelectionDown else C.moveSelectionUp
    in case fn c of
        Just p  -> s {tasks = p}
        Nothing -> s

-- | insert a new Item
newItem :: TuiState -> TuiState
newItem s =
    let nxID = nID s
        t = head $ BE.getEditContents $ _insertEditor s
        new = Item {iID = nxID, checked = False, text = t}
    in if t /= ""
        then s {tasks = (case insertLocal s of
            Below   -> C.insertAbove
            Above   -> C.insertBelow
            Bottom  -> C.insertTop
            Top     -> C.insertBottom) 
        (tasks s) new, nID = nxID + 1}
        else s

removeItem :: TuiState -> TuiState
removeItem s =
    let c = C.removeSelectedFocusNext $ tasks s
        e = C.isEmpty c
    in s {tasks = c, showEditor = e}

-- | toggle checked status on selection
toggleCheck :: TuiState -> TuiState
toggleCheck s =
    let c = tasks s
    in case C.selected c of
        Just sel -> let update = sel {checked = not $ checked sel} in
            s {tasks = c {selected = Just update}}
        Nothing -> s


{-------------------------------------------------------------------------------------------------
                                            Editor
-------------------------------------------------------------------------------------------------}
-- | reset Editor and State after insert/cancel
resetEditor :: TuiState -> TuiState
resetEditor s = s
    { _insertEditor = BE.editor "Insert" (Just 1) ""
    , showEditor = False
    , insertLocal = Bottom
    }

{-------------------------------------------------------------------------------------------------
                                            Events
-------------------------------------------------------------------------------------------------}
-- | handle input events
handleTuiEvent :: TuiState -> BrickEvent String e -> EventM String (Next TuiState)
handleTuiEvent s e =
    if showEditor s == False
        then case e of
            -- KeyEvent
            VtyEvent vtye ->
                case vtye of
                    -- quit
                    EvKey (KChar 'q') []    -> halt s
                    EvKey KEsc []           -> halt s
                    -- move down
                    EvKey (KChar 'k') []    -> continue $ updateSelection Down s
                    EvKey KDown []          -> continue $ updateSelection Down s
                    -- move up
                    EvKey (KChar 'j') []    -> continue $ updateSelection Up s
                    EvKey KUp []            -> continue $ updateSelection Up s
                    -- move selected item down
                    EvKey (KChar 'K') []    -> updateFile $ moveSelection Down s
                    EvKey KDown [MShift]    -> updateFile $ moveSelection Down s
                    -- move selected item up
                    EvKey (KChar 'J') []    -> updateFile $ moveSelection Up s
                    EvKey KUp [MShift]      -> updateFile $ moveSelection Up s
                    -- toggle Items checked state
                    EvKey (KChar 'c') []    -> updateFile $ toggleCheck s
                    -- delete Item
                    EvKey (KChar 'd') []    -> updateFile $ removeItem s
                    -- insert new Item above selection
                    EvKey (KChar 'i') []    -> continue $ s {insertLocal = Above, showEditor = True }
                    -- insert new Item top of List
                    EvKey (KChar 'I') []    -> continue $ s {insertLocal = Top, showEditor = True }
                    -- append new Item below selection
                    EvKey (KChar 'a') []    -> continue $ s {insertLocal = Below, showEditor = True } 
                    -- append new Item to bottom of List
                    EvKey (KChar 'A') []    -> continue $ s {insertLocal = Bottom, showEditor = True }
                    _ -> continue s
            _ -> continue s
        else handleInsertEvent s e

-- | handle events while in Editor
handleInsertEvent :: TuiState -> BrickEvent String e -> EventM String (Next TuiState)
handleInsertEvent s e = case e of
    VtyEvent vtye ->
        case vtye of
            -- cancel insert, reset editor
            EvKey KEsc [] -> if C.isEmpty $ tasks s
                then halt s
                else continue $ resetEditor s
            -- insert new item
            EvKey KEnter [] -> updateFile $ resetEditor $ newItem s
            -- everything else
            _ -> continue =<< handleEventLensed s insertEditor BE.handleEditorEvent vtye
    _ -> continue s