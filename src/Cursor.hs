module Cursor
    ( Cursor (..)
    , fromList
    , fromListSelect
    , toList
    , selectFirst
    , selectLast
    , selectNext
    , selectPrev
    , insertAbove
    , insertTop
    , insertBelow
    , insertBottom
    , isEmpty
    , moveSelectionUp
    , moveSelectionDown
    , removeSelected
    , removeSelectedFocusNext
    , removeSelectedFocusPrev
    )
    where

data Cursor a = Cursor
    { previous :: [a] -- reverse order
    , selected :: Maybe a
    , next :: [a]
    } deriving (Show, Eq)

fromList :: [a] -> Cursor a
fromList [] = empty
fromList (x : xs) = Cursor {previous = xs, selected = Just x, next = []}

fromListSelect :: Int -> [a] -> Maybe (Cursor a)
fromListSelect index li = if length li < index + 1 || index < 0
    then Nothing
    else let (n, p) = splitAt index li
        in Just $ Cursor {previous = reverse n, selected = Just (head p), next = tail p}

toList :: Cursor a -> [a]
toList c = let  n = next c
                s = selected c
                p = reverse $ previous c
    in case s of
        Just sel -> reverse $  p ++ [sel] ++ n
        Nothing -> []

selectFirst :: Cursor a -> Maybe (Cursor a)
selectFirst c = case toList c of
    [] -> Nothing
    li -> fromListSelect 0 li

selectLast :: Cursor a -> Maybe (Cursor a)
selectLast c = case toList c of
    [] -> Nothing
    li -> fromListSelect (length li - 1) li

selectNext :: Ord a => Cursor a -> Maybe (Cursor a)
selectNext c = if next c == []
    then Nothing
    else let    cNext = next c
                cPrev = previous c
                Just cSel = selected c  
        in Just $ Cursor {next = tail cNext, selected = Just $ head cNext, previous = cSel : cPrev} 

selectPrev :: Ord a => Cursor a -> Maybe (Cursor a)
selectPrev c = if previous c == []
    then Nothing
    else let    cNext = next c
                cPrev = previous c
                Just cSel = selected c  
        in Just $ Cursor {next = cSel : cNext, selected = Just $ head cPrev, previous = tail cPrev} 

insertTop :: Cursor a -> a -> Cursor a
insertTop c a = case selected c of
    Just _  -> c {previous = (previous c) ++ [a]}
    Nothing -> empty {selected = Just a}

insertBottom :: Cursor a -> a -> Cursor a
insertBottom c a = case selected c of
    Just _  -> c {next = (next c) ++ [a]}
    Nothing -> empty {selected = Just a}


insertAbove :: Cursor a -> a -> Cursor a
insertAbove c a = let p = previous c
    in case selected c of
        Nothing -> c {selected = Just a}
        Just _ -> c {previous = a : p}

insertBelow :: Cursor a -> a -> Cursor a
insertBelow c a = let n = next c
    in case selected c of
        Nothing -> c {selected = Just a}
        Just _ -> c {next = a : n}

moveSelectionDown :: Cursor a -> Maybe (Cursor a)
moveSelectionDown c =
    let p = previous c
        n = next c 
    in case selected c of
        Just _ -> if length p > 0
            then Just $ c {previous = tail p, next = head p : n}
            else Nothing
        Nothing -> Nothing

moveSelectionUp :: Cursor a -> Maybe (Cursor a)
moveSelectionUp c =
    let p = previous c
        n = next c 
    in case selected c of
        Just _ -> if length n > 0
            then Just $ c {previous = head n : p, next = tail n}
            else Nothing
        Nothing -> Nothing

removeSelected :: Cursor a -> Cursor a
removeSelected c = case selected c of
    Nothing -> c
    Just _ -> let   n = next c
                    p = previous c
        in fromList $ (reverse p) ++ n

removeSelectedFocusNext :: Ord a => Cursor a -> Cursor a
removeSelectedFocusNext c = case selectNext c of
    Just n -> let prev = previous n in n {previous = tail prev}
    Nothing -> if previous c == []
        then empty
        else let prev = previous c in Cursor {next = [], selected = Just $ head prev, previous = tail prev}

removeSelectedFocusPrev :: Ord a => Cursor a -> Cursor a
removeSelectedFocusPrev c = case selectPrev c of
    Just prev -> let n = next prev in prev {next = tail n}
    Nothing -> if next c == []
        then empty
        else let n = next c in Cursor {next = tail n, selected = Just $ head n, previous = []}

isEmpty :: Cursor a -> Bool
isEmpty c = case selected c of
    Just _ -> False
    Nothing -> True

empty :: Cursor a
empty = Cursor {next = [], selected = Nothing, previous = []}