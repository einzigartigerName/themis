{-# LANGUAGE OverloadedStrings #-}

module Attr where

import Config (CColors (..))

import Brick.AttrMap
import Brick.Util (on)
import Brick.Widgets.Border (borderAttr)
import Brick.Widgets.Edit (editFocusedAttr)
import Graphics.Vty.Attributes


buildAttrMap :: CColors -> [(AttrName, Attr)]
buildAttrMap c =
    [ (itemAttr,            itemFG c      `on` itemBG c)
    , (selAttr,             selFG c       `on` selBG c)
    , (fileAttr,            fileFG c      `on` fileBG c)
    , (editorBorderAttr,    editBordFG c  `on` editBordBG c)
    , (editorLableAttr,     editLableFG c `on` editLableBG c)
    , (editorAttr,          editFG c      `on` editBG c)
    ]

itemAttr :: AttrName
itemAttr = "item"

selAttr :: AttrName
selAttr = "selected"

fileAttr :: AttrName
fileAttr = "file"

editorBorderAttr :: AttrName
editorBorderAttr = borderAttr

editorLableAttr :: AttrName
editorLableAttr = "editorLable"

editorAttr :: AttrName
editorAttr = editFocusedAttr
