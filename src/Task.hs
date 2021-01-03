module Task where

import Data.List
import Brick.Types
import Brick.Widgets.Core

type ID = Int
type Tasks = [Item]

data Item = Item
    { depth :: !Int  
    , iID :: !Int
    , checked :: !Bool
    , text :: !String
    } deriving (Eq, Ord)

instance Show Item where
    show e = "\nEntry:\n\tID: " ++ show (iID e) ++ "\n\tchecked: " ++ show (checked e) ++ "\n\ttext: " ++ show (text e) ++ "\n"

nextID :: Tasks -> ID
nextID es = foldr (\x acc -> if iID x > acc then iID x else acc) 0 es + 1

-- | returnes an entry with the given ID if one exists
getItemById :: Tasks -> ID -> Maybe Item
getItemById [] _ = Nothing
getItemById (e : es) eid = if iID e == eid then Just e else getItemById es eid

-- | add an entry to the Tasks
addItem :: Tasks -> Item -> Tasks
addItem es entry = entry : es

-- | remove an entry from the tasks with matching ID
removeItemById :: Tasks -> ID -> Tasks
removeItemById entries rid = filter (\x -> iID x /= rid) entries

indent :: Int -> String
indent 0 = ""
indent d = "  " ++ indent (d - 1)

-- | serialize Tasks to Text
serializeF :: Tasks -> String
serializeF = foldl' serializeItemF ""

-- | serialize an Entry to Text
serializeItemF :: String -> Item -> String
serializeItemF acc e =
    let i = indent $ depth e 
        done = if checked e then "[x] " else "[ ] " in
        concat [acc, i, done, text e, "\n"]

serializeW :: Tasks -> [Widget n]
serializeW = map serializeItemW

serializeItemW :: Item -> Widget n
serializeItemW e = str $ (if checked e then "✓ " else "  ") ++  indent (depth e) ++ text e
