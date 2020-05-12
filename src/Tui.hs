{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Tui where

import Task as T
import Cursor as C

import Lens.Micro.TH

import Brick.AttrMap
import Brick.Main
import Brick.Types
import Brick.Util
import Brick.Widgets.Edit as BE
import Brick.Widgets.Core

import Graphics.Vty.Input.Events
import Graphics.Vty.Attributes

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
tui :: Tasks -> FilePath -> IO ()
tui ts fp = do
    initialState <- buildInitialState ts fp
    _ <- defaultMain tuiApp initialState
    return ()

-- | Build Tui App
tuiApp :: App TuiState e String
tuiApp =
    App
        { appDraw = drawTui
        , appChooseCursor = showFirstCursor
        , appHandleEvent = handleTuiEvent
        , appStartEvent = pure
        , appAttrMap = const $ attrMap mempty [("selected", bg cyan)]
        }

-- | draw Tui
drawTui :: TuiState -> [Widget String]
drawTui ts = let c = tasks ts
    in if showEditor ts == True
        -- render editor
        then [BE.renderEditor (str . unlines) True $ _insertEditor ts]
        -- render list
        else case C.selected c of
            Nothing -> [BE.renderEditor (str . unlines) True $ _insertEditor ts]
            Just s -> [vBox $
                concat
                    [ map (drawItem False) $ reverse $ C.next c 
                    , [drawItem True $ s]
                    , map (drawItem False) $ C.previous c
                    ]
                ]

-- | Draw Item in Todo List
drawItem :: Bool -> Item -> Widget n
drawItem s =
    (if s
        then withAttr "selected"
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
    in
    s {tasks = (case insertLocal s of
        Below   -> C.insertAbove
        Above   -> C.insertBelow
        Bottom  -> C.insertTop
        Top     -> C.insertBottom) 
    (tasks s) new, nID = nxID + 1}

-- | toggle checked status on selection
toggleCheck :: TuiState -> TuiState
toggleCheck ts =
    let c = tasks ts
    in case C.selected c of
        Just sel -> let update = sel {checked = not $ checked sel} in
            ts {tasks = c {selected = Just update}}
        Nothing -> ts


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
            VtyEvent vtye ->
                case vtye of
                    -- quit
                    EvKey (KChar 'q') []    -> halt s
                    -- move down
                    EvKey (KChar 'k') []    -> continue $ updateSelection Down s
                    EvKey KDown []          -> continue $ updateSelection Down s
                    -- move up
                    EvKey (KChar 'j') []    -> continue $ updateSelection Up s
                    EvKey KUp []            -> continue $ updateSelection Up s
                    -- move selected item down
                    EvKey (KChar 'K') []    -> continue $ moveSelection Down s
                    EvKey KDown [MShift]    -> continue $ moveSelection Down s
                    -- move selected item up
                    EvKey (KChar 'J') []    -> continue $ moveSelection Up s
                    EvKey KUp [MShift]      -> continue $ moveSelection Up s
                    -- toggle Items checked state
                    EvKey (KChar 'c') []    -> continue $ toggleCheck s
                    -- delete Item
                    EvKey (KChar 'd') []    -> do
                        let c = tasks s
                        continue $ s {tasks = C.removeSelectedFocusNext c}
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
            EvKey KEsc [] -> continue $ resetEditor s
            -- insert new item
            EvKey KEnter [] -> continue $ resetEditor $ newItem s
            -- everything else
            _ -> continue =<< handleEventLensed s insertEditor BE.handleEditorEvent vtye
    _ -> continue s