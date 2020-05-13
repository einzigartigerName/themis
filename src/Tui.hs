{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Tui
    ( TuiState (..)
    , tui
    )
    where

import Task as T
import FileIO as F

import Lens.Micro.TH

import Brick.AttrMap
import Brick.Main
import Brick.Types
import Brick.Util
import Brick.Widgets.Edit as BE
import Brick.Widgets.List as BL
import Brick.Widgets.Center as BC
import Brick.Widgets.Border as BB
import Brick.Widgets.Core

import Control.Monad.IO.Class (liftIO)

import qualified Data.Vector as Vec

import Graphics.Vty.Input.Events
import Graphics.Vty.Attributes


{-------------------------------------------------------------------------------------------------
                                            DataTypes
-------------------------------------------------------------------------------------------------}
data InsertLocation = Top | Bottom | Above | Below
    deriving Eq

data WidgetName = WEditor | WList
    deriving (Eq, Ord, Show)

data TuiState = TuiState
    { _tasks :: BL.List WidgetName Item 
    , file :: FilePath
    , nID :: Int
    , _insertEditor :: BE.Editor String WidgetName
    , showEditor :: Bool
    , insertLocal :: InsertLocation
    }
makeLenses ''TuiState

{-------------------------------------------------------------------------------------------------
                                            Tui
-------------------------------------------------------------------------------------------------}
-- | Main call, start Tui
tui :: Tasks -> FilePath -> IO TuiState
tui ts fp = do
    initialState <- buildInitialState ts fp
    defaultMain tuiApp initialState

-- | Build Tui App
tuiApp :: App TuiState e WidgetName
tuiApp =
    App
        { appDraw = drawTui
        , appChooseCursor = showFirstCursor
        , appHandleEvent = handleTuiEvent
        , appStartEvent = pure
        , appAttrMap = const $ attrMap mempty attrMappings
        }

{-------------------------------------------------------------------------------------------------
                                            State
-------------------------------------------------------------------------------------------------}
-- | Init TuiState
buildInitialState :: Tasks -> FilePath -> IO TuiState
buildInitialState ts fp =
    --let cursor = C.fromList ts in
    pure TuiState
        { _tasks = BL.list WList (Vec.fromList ts) 1
        , file = fp
        , nID = T.nextID ts
        , _insertEditor = BE.editor WEditor (Just 1) ""
        , showEditor = ts == []
        , insertLocal = Bottom
        }

-- | writes changes to file
updateFile :: TuiState -> EventM WidgetName (Next TuiState)
updateFile s =
    let fpath = file s
        items = Vec.toList $ BL.listElements $ _tasks s
    in do
        liftIO $ F.writeItemsToFile fpath items
        continue s


{-------------------------------------------------------------------------------------------------
                                            Tui Rendering
-------------------------------------------------------------------------------------------------}
titleAttr :: AttrName
titleAttr = "title"

attrMappings :: [(AttrName, Attr)]
attrMappings =
    [ (BL.listSelectedAttr, bg cyan)
    , (BB.borderAttr,   yellow `on` black)
    , (titleAttr,       fg cyan)
    ]

-- | draw Tui
drawTui :: TuiState -> [Widget WidgetName]
drawTui s =
    let e = showEditor s
        li = _tasks s
        lRender = [BL.renderList drawItem True li]
        empty = null $ listElements li
    in if e || empty
        then (drawEditor $ _insertEditor s) ++ lRender
        else lRender
--
-- | draw editor
drawEditor :: Editor String WidgetName -> [Widget WidgetName]
drawEditor e = 
    [ BC.centerLayer $
      BB.borderWithLabel
        (BB.vBorder <+> (withAttr titleAttr $ str " New Item ") <+> BB.vBorder) $
      hLimit 40 $
      vLimit 3 $
      withAttr "editor" $ 
      BE.renderEditor (str . unlines) True e
    ]

drawItem :: Bool -> Item -> Widget WidgetName
drawItem sel =
    (if sel
        then withAttr BL.listSelectedAttr
        else id) .
    serializeItemW


{-------------------------------------------------------------------------------------------------
                                            items
-------------------------------------------------------------------------------------------------}
-- | move selection
moveSelection :: Direction -> TuiState -> TuiState
moveSelection d s =
    let li = _tasks s
        posM = BL.listSelected li
    in case posM of
        Just pos -> case indexNext li d of
            Just switch -> s {_tasks = switchItems li pos switch}
            Nothing -> s
        Nothing -> s



-- | insert a new Item
newItem :: TuiState -> TuiState
newItem s =
    let nxID = nID s
        content = head $ BE.getEditContents $ _insertEditor s
        new = Item {iID = nxID, checked = False, text = content}
        li = _tasks s
        pos = insertPosition (insertLocal s) li
    in if content /= ""
        then s {_tasks = BL.listInsert pos new li}
        else s

-- | calc insert index
insertPosition :: InsertLocation -> BL.List WidgetName Item -> Int
insertPosition pos li = case pos of
    Above -> case BL.listSelected li of 
        Just i -> i
        Nothing -> 0  
    Below -> case BL.listSelected li of 
        Just i -> i + 1
        Nothing -> 0 
    Top -> 0
    Bottom -> Vec.length $ BL.listElements li

-- | remove selected Item
removeItem :: TuiState -> TuiState
removeItem s =
    let li = _tasks s
        posM = BL.listSelected li
        e = (Vec.length $ listElements li) - 1 <= 0
    in case posM of
        Just pos -> s {_tasks = BL.listRemove pos li, showEditor = e}
        Nothing -> s

-- | toggle checked status on selection
toggleCheck :: TuiState -> TuiState
toggleCheck s =
    let toggle item = item {checked = not $ checked item}
        li = _tasks s
    in s {_tasks = BL.listModify toggle li}


{-------------------------------------------------------------------------------------------------
                                            List
-------------------------------------------------------------------------------------------------}
-- | is List empty
isEmpty :: BL.List WidgetName Item -> Bool
isEmpty li = Vec.null $ BL.listElements li

-- | get length of list
listLength :: BL.List WidgetName Item -> Int
listLength li = Vec.length $ BL.listElements li

-- | get next Index
indexNext :: BL.List WidgetName Item -> Direction -> Maybe Int
indexNext li dir = let len = listLength li 
    in if isEmpty li || len < 2
        then Nothing
        else let Just index =  BL.listSelected li
            in case dir of
                Up -> if index - 1 < 0 then Nothing else Just $ index - 1 
                Down -> if index + 1 >= len then Nothing else Just $ index + 1

-- | switch two items in list
switchItems :: BL.List WidgetName Item -> Int -> Int -> BL.List WidgetName Item
switchItems li a b =
    let content = BL.listElements li
        current = content Vec.! a
        switch = content Vec.! b
        updated = content Vec.// [(a, switch), (b, current)]
    in BL.listReplace updated (Just b) li
{-------------------------------------------------------------------------------------------------
                                            Editor
-------------------------------------------------------------------------------------------------}
-- | reset Editor and State after insert/cancel
resetEditor :: TuiState -> TuiState
resetEditor s = s
    { _insertEditor = BE.editor WEditor (Just 1) ""
    , showEditor = False
    , insertLocal = Bottom
    }

{-------------------------------------------------------------------------------------------------
                                            Events
-------------------------------------------------------------------------------------------------}
-- | handle input events
handleTuiEvent :: TuiState -> BrickEvent WidgetName e -> EventM WidgetName (Next TuiState)
handleTuiEvent s e =
    if not $ showEditor s
        then case e of
            -- KeyEvent
            VtyEvent vtye ->
                case vtye of
                    -- quit
                    EvKey (KChar 'q')   []  -> halt s
                    EvKey KEsc          []  -> halt s
                    -- move selected item down
                    EvKey (KChar 'K') []    -> updateFile $ moveSelection Down s
                    EvKey KDown [MShift]    -> updateFile $ moveSelection Down s
                    -- move selected item up
                    EvKey (KChar 'J') []    -> updateFile $ moveSelection Up s
                    EvKey KUp [MShift]      -> updateFile $ moveSelection Up s
                    -- toggle checked
                    EvKey (KChar 'c') []    -> updateFile $ toggleCheck s
                    -- delete Item
                    EvKey (KChar 'd') []    -> updateFile $ removeItem s
                    -- insert new Item above selection
                    EvKey (KChar 'i') []    -> updateFile $ s {insertLocal = Above, showEditor = True }
                    -- insert new Item top of List
                    EvKey (KChar 'I') []    -> updateFile $ s {insertLocal = Top, showEditor = True }
                    -- append new Item below selection
                    EvKey (KChar 'a') []    -> updateFile $ s {insertLocal = Below, showEditor = True } 
                    -- append new Item to bottom of List
                    EvKey (KChar 'A') []    -> updateFile $ s {insertLocal = Bottom, showEditor = True }
                    -- handle other events
                    ev -> continue =<< handleEventLensed s tasks (BL.handleListEventVi BL.handleListEvent) ev
                    -- _ -> continue s
            _ -> continue s
        else handleInsertEvent s e

-- | handle events while in Editor
handleInsertEvent :: TuiState -> BrickEvent WidgetName e -> EventM WidgetName (Next TuiState)
handleInsertEvent s e = case e of
    VtyEvent vtye ->
        case vtye of
            -- cancel insert, reset editor
            EvKey KEsc [] -> if isEmpty $ _tasks s
                then halt s
                else continue $ resetEditor s
            -- insert new item
            EvKey KEnter [] -> updateFile $ resetEditor $ newItem s
            -- everything else
            _ -> continue =<< handleEventLensed s insertEditor BE.handleEditorEvent vtye
    _ -> continue s