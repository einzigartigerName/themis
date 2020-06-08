{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Tui
    ( TuiState (..)
    , tui
    )
    where

import Task as T
import FileIO as F
import Config
import Attr

import Lens.Micro
import Lens.Micro.TH

import Brick.AttrMap
import Brick.Main
import Brick.Types
import qualified Brick.Widgets.Edit as BE
import qualified Brick.Widgets.List as BL
import qualified Brick.Widgets.Center as BC
import qualified Brick.Widgets.Border as BB
import Brick.Widgets.Core

import Control.Monad.IO.Class (liftIO)

import qualified Data.Vector as Vec

import Graphics.Vty.Input.Events
import Graphics.Vty.Attributes

import Text.Printf


{-------------------------------------------------------------------------------------------------
                                            DataTypes
-------------------------------------------------------------------------------------------------}
data InsertLocation = Top | Bottom | Above | Below
    deriving Eq

data WidgetName = WEditor | WList | WHelp
    deriving (Eq, Ord, Show)

data TuiState = TuiState
    { keybinds :: CKeys
    , _tasks :: BL.List WidgetName Item
    , file :: FilePath
    , nID :: Int
    , _itemEditor :: BE.Editor String WidgetName
    , helpV :: ViewportScroll WidgetName
    , insertLocal :: InsertLocation
    , showEditor :: Bool
    , editItem :: Bool
    , helpMode :: Bool
    }
makeLenses ''TuiState


{-------------------------------------------------------------------------------------------------
                                            Tui
-------------------------------------------------------------------------------------------------}
-- | Main call, start Tui
tui :: Tasks                -- Tasks 
    -> CColors              -- Colors
    -> CKeys                -- Keybinds
    -> FilePath             -- File to use
    -> IO TuiState          -- Exit State
tui ts colors keys fp = do
    initialState <- buildInitialState ts keys fp
    defaultMain (tuiApp $ buildAttrMap colors) initialState

-- | Build Tui App
tuiApp :: [(AttrName, Attr)] -> App TuiState e WidgetName
tuiApp attr =
    App
        { appDraw = drawTui
        , appChooseCursor = showFirstCursor
        , appHandleEvent = handleTuiEvent
        , appStartEvent = pure
        , appAttrMap = const $ attrMap mempty attr
        }


{-------------------------------------------------------------------------------------------------
                                            State
-------------------------------------------------------------------------------------------------}
-- | Init TuiState
buildInitialState :: Tasks -> CKeys -> FilePath -> IO TuiState
buildInitialState ts keys fp =
    pure TuiState
        { keybinds = keys
        , _tasks = BL.list WList (Vec.fromList ts) 1
        , file = fp
        , nID = T.nextID ts
        , _itemEditor = BE.editor WEditor (Just 1) ""
        , helpV = viewportScroll WHelp
        , showEditor = ts == []
        , editItem = False
        , insertLocal = Bottom
        , helpMode = False
        }

-- | writes changes to file
updateFile :: TuiState -> EventM WidgetName (Next TuiState)
updateFile s =
    let fpath = file s
        items = Vec.toList $ BL.listElements $ _tasks s
    in do
        liftIO $ F.writeItemsToFile fpath items
        continue s


-- | prepare State for Item Editing
prepareEditItem :: TuiState -> TuiState
prepareEditItem s =
    let li = _tasks s
        sContent = case BL.listSelectedElement li of
            Just (_, item) -> text item
            Nothing -> ""
    in s {editItem = True, showEditor = True, _itemEditor = setEditorContent sContent}


{-------------------------------------------------------------------------------------------------
                                            Tui Rendering
-------------------------------------------------------------------------------------------------}
-- | draw Tui
drawTui :: TuiState -> [Widget WidgetName]
drawTui s =
    let e = showEditor s
        li = _tasks s
        lRender = [padBottom (Pad 2) $ BL.renderList drawItem True li]
        empty = null $ BL.listElements li
        filePath = [drawFileLayer s]
    in if not $ helpMode s
        then if e || empty
            then filePath ++ (drawEditor (editItem s) (_itemEditor s)) ++ lRender
            else filePath ++ lRender
        else drawHelp s

-- | draw editor
drawEditor :: Bool -> BE.Editor String WidgetName -> [Widget WidgetName]
drawEditor b e = 
    let title = if b then " Edit Item " else " New Item "
    in
    [ BC.centerLayer $
      hLimitPercent 70 $
      
      BB.borderWithLabel
        (BB.vBorder <+> (withAttr editorLableAttr $ str title) <+> BB.vBorder) $
      withAttr editorAttr $ 
      BE.renderEditor (str . unlines) True e
    ]

drawItem :: Bool -> Item -> Widget WidgetName
drawItem sel =
    (if sel
        then withAttr selAttr
        else withAttr itemAttr) .
    serializeItemW

-- | draw Row at bottom to Show FilePath
drawFileLayer :: TuiState -> Widget WidgetName
drawFileLayer s = Widget Fixed Fixed $ do
    c <- getContext
    let h = c^.availHeightL
    render $ translateBy (Location (0, h-1)) $
        withAttr fileAttr $
        str ("Using File: " <> file s)

-- | draw help page
drawHelp :: TuiState -> [Widget WidgetName]
drawHelp s = drawHelpLayer : [viewport WHelp Both $
    withAttr itemAttr $ vBox $ drawKeys $ keybinds s]

-- | draw keybindings
drawKeys :: CKeys -> [Widget WidgetName]
drawKeys k =
    [ str (printf "%-20s %s" ("Insert Above"::String)   (printKeybind $ insert k))
    , str (printf "%-20s %s" ("Insert Top"::String)     (printKeybind $ top k))
    , str (printf "%-20s %s" ("Insert Above"::String)   (printKeybind $ append k))
    , str (printf "%-20s %s" ("Insert Bottom"::String)  (printKeybind $ bottom k))
    , str (printf "%-20s %s" ("Edit"::String)           (printKeybind $ editMode k))
    , str (printf "%-20s %s" ("Delete"::String)         (printKeybind $ delete k))
    , str (printf "%-20s %s" ("Toggle Checked"::String) (printKeybind $ check k))
    , str (printf "%-20s %s" ("Up"::String)             (printKeybind $ up k))
    , str (printf "%-20s %s" ("Down"::String)           (printKeybind $ down k))
    , str (printf "%-20s %s" ("Move Up"::String)        (printKeybind $ moveUp k))
    , str (printf "%-20s %s" ("MoveDown"::String)       (printKeybind $ moveDown k))
    , str (printf "%-20s %s" ("Help"::String)           (printKeybind $ help k))
    , str (printf "%-20s %s" ("Quit"::String)           (printKeybind $ quit k))
    ]

drawHelpLayer :: Widget WidgetName
drawHelpLayer =  Widget Fixed Fixed $ do
    c <- getContext
    let h = c^.availHeightL
    render $ translateBy (Location (0, h-1)) $
        withAttr fileAttr $
        str "Quit Help: q"
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

-- | update items (insert/edit Item)
updateItems :: TuiState -> TuiState
updateItems s = if editItem s
    then editSelectedItem s
    else newItem s

-- | edit the current seleted item
editSelectedItem :: TuiState -> TuiState
editSelectedItem s =
    let edit item = item {text = head $ BE.getEditContents $ _itemEditor s}
        li = _tasks s
    in s {_tasks = BL.listModify edit li}

-- | insert a new Item
newItem :: TuiState -> TuiState
newItem s =
    let nxID = nID s
        content = head $ BE.getEditContents $ _itemEditor s
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
        e = (Vec.length $ BL.listElements li) - 1 <= 0
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
    { _itemEditor = BE.editor WEditor (Just 1) ""
    , showEditor = False
    , editItem = False
    , insertLocal = Bottom
    }

-- | get the content of the editor
setEditorContent :: String -> BE.Editor String WidgetName
setEditorContent = BE.editor WEditor (Just 1)
{-------------------------------------------------------------------------------------------------
                                            Events
-------------------------------------------------------------------------------------------------}
-- | handle input events
handleTuiEvent :: TuiState -> BrickEvent WidgetName e -> EventM WidgetName (Next TuiState)
handleTuiEvent s e =
    if not $ showEditor s
        then if helpMode s
            then handleHelpEvent s e
            else case e of
                -- KeyEvent
                VtyEvent vtye -> matchEvent s (keybinds s) vtye
                _ -> continue s
        else handleInsertEvent s e

-- | handle events while in Editor
handleHelpEvent :: TuiState -> BrickEvent WidgetName e -> EventM WidgetName (Next TuiState)
handleHelpEvent s e = case e of
    VtyEvent vtye ->
        case vtye of
            -- cancel insert, reset editor
            EvKey KEsc [] -> continue $ s {helpMode = False}
            EvKey (KChar 'q') [] -> continue $ s {helpMode = False}
            -- insert new item
            EvKey KDown []  -> vScrollBy (helpV s) 1 >> continue s
            EvKey KUp []    -> vScrollBy (helpV s) (-1) >> continue s
            EvKey KRight [] -> hScrollBy (helpV s) 1 >> continue s
            EvKey KLeft []  -> hScrollBy (helpV s) (-1) >> continue s
            -- jsut continue
            _ -> continue s
    _ -> continue s

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
            EvKey KEnter [] -> updateFile $ resetEditor $ updateItems s
            -- everything else
            _ -> continue =<< handleEventLensed s itemEditor BE.handleEditorEvent vtye
    _ -> continue s

-- | match event with keybinds
matchEvent :: TuiState -> CKeys -> Event ->  EventM WidgetName (Next TuiState)
matchEvent s k e
    -- quit
    | e == quit k               = halt s
    | e == EvKey KEsc []        = halt s
    | e == help k               = continue $ s {helpMode = True}
    -- move selected Item up
    | e == moveUp k             = updateFile $ moveSelection Up s
    | e == EvKey KUp [MShift]   = updateFile $ moveSelection Up s
    -- moce selected Item down
    | e == moveDown k           = updateFile $ moveSelection Down s
    | e == EvKey KDown [MShift] = updateFile $ moveSelection Down s
    -- selction up
    | e == up k                 = continue $ s {_tasks = BL.listMoveUp $ _tasks s}
    -- selectoin down
    | e == down k               = continue $ s {_tasks = BL.listMoveDown $ _tasks s}
    -- toggle check status
    | e == check k              = updateFile $ toggleCheck s
    -- delete selection
    | e == delete k             = updateFile $ removeItem s
    -- edit selection
    | e == editMode k           = continue $ prepareEditItem s
    -- insertions
    | e == insert k             = continue $ s {insertLocal = Above, showEditor = True }
    | e == top k                = continue $ s {insertLocal = Top, showEditor = True }
    | e == append k             = continue $ s {insertLocal = Below, showEditor = True }
    | e == bottom k                = continue $ s {insertLocal = Bottom, showEditor = True }
    -- other events; default List events
    | otherwise                 = continue =<< handleEventLensed s tasks (BL.handleListEventVi BL.handleListEvent) e

