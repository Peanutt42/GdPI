{-# OPTIONS_GHC -Wno-x-partial -Wno-unrecognised-warning-flags #-}

--
-- SOURCE: Frederik Naumann <frederik.naumann@uni-ulm.de>
--

--
--------------------------------------------------------------------------------
-- 1. FOLD FUNCTION
-- Task: Define a fold function for a custom recursive data type
data BinTree a
  = Node a (BinTree a) (BinTree a)
  | Leaf a
  | Empty
  deriving (Show, Eq)

-- foldTree :: (a -> b -> b -> b) -> (a -> b) -> b -> BinTree a -> b
foldTree :: (a -> b -> b -> b) -> (a -> b) -> b -> BinTree a -> b
foldTree fNode fLeaf fEmpty = go
  where
    go (Node x l r) = fNode x (go l) (go r)
    go (Leaf x) = fLeaf x
    go Empty = fEmpty

--------------------------------------------------------------------------------
-- 2. UNEITHER
-- Task: Split a list of Either into two lists (Lefts and Rights)
uneither :: [Either a b] -> ([a], [b])
uneither [] = ([], [])
uneither (x : xs) =
  let (ls, rs) = uneither xs
   in case x of
        Left l -> (l : ls, rs)
        Right r -> (ls, r : rs)

--------------------------------------------------------------------------------
-- 3. COUNTING NODES/LEAVES IN A TREE
-- Task: Count the number of leaves in a binary tree
countLeaves :: BinTree a -> Int
countLeaves (Node _ l r) = countLeaves l + countLeaves r
countLeaves (Leaf _) = 1
countLeaves Empty = 0

--------------------------------------------------------------------------------
-- 4. LIST2SET / TOSET
-- Task: Remove duplicates from a list (order may not be preserved)
list2set :: (Eq a) => [a] -> [a]
list2set [] = []
list2set (x : xs) = x : filter (/= x) (list2set xs)

--------------------------------------------------------------------------------
-- 5. SUMTREE / PRODUCTTREE
-- Task: Sum or multiply all values in a tree using fold
sumTree :: (Num a) => BinTree a -> a
sumTree = foldTree (\x l r -> x + l + r) id 0

productTree :: (Num a) => BinTree a -> a
productTree = foldTree (\x l r -> x * l * r) id 1

--------------------------------------------------------------------------------
-- 6. MAYBEFILTER / EITHERFILTER
-- Task: Apply a predicate and wrap results in Maybe/Either
maybeFilter :: (a -> Bool) -> [a] -> [Maybe a]
maybeFilter p = map (\x -> if p x then Just x else Nothing)

maybeFilter' :: (a -> Bool) -> [a] -> [Maybe a]
maybeFilter' p xs = [if p x then Just x else Nothing | x <- xs]

eitherFilter :: (a -> Bool) -> [a] -> [Either a a]
eitherFilter p = map (\x -> if p x then Right x else Left x)

--------------------------------------------------------------------------------
-- 7. REMOVELEAVES
-- Task: Remove all leaves from a tree
removeLeaves :: BinTree a -> BinTree a
removeLeaves = foldTree Node (const Empty) Empty

--------------------------------------------------------------------------------
-- 8. LEAFIFY
-- Task: Convert nodes without children to leaves
leafify :: (Eq a) => BinTree a -> BinTree a
leafify =
  foldTree
    ( \x l r -> case (l, r) of
        (Empty, Empty) -> Leaf x
        _ -> Node x l r
    )
    Leaf
    Empty

--------------------------------------------------------------------------------
-- 9. SELECT / SELECTS
-- Task: Select first element matching predicate and return it with the rest
select :: (a -> Bool) -> [a] -> Maybe (a, [a])
select _ [] = Nothing
select p (x : xs)
  | p x = Just (x, xs)
  | otherwise = case select p xs of
      Nothing -> Nothing
      Just (y, ys) -> Just (y, x : ys)

--------------------------------------------------------------------------------
-- 10. HASHING FUNCTIONS (QSUM AND H)
-- Task: Compute digit sum and hash value modulo 11
qsum :: Int -> Int
qsum x
  | x < 0 = 0
  | x < 10 = x
  | otherwise = x `mod` 10 + qsum (x `div` 10)

h :: Int -> Int
h x = qsum x `mod` 11

--------------------------------------------------------------------------------
-- 11. POS / FINDINDEX
-- Task: Find the first index of an element in a list
findIndex :: (Eq a) => a -> [a] -> Maybe Int
findIndex e xs = go 0 xs
  where
    go _ [] = Nothing
    go i (y : ys)
      | e == y = Just i
      | otherwise = go (i + 1) ys

--------------------------------------------------------------------------------
-- 12. FOO' (UNZIP TRIPLES)
-- Task: Convert a list of triples into three separate lists
unzipTriples :: [(a, b, c)] -> ([a], [b], [c])
unzipTriples [] = ([], [], [])
unzipTriples ((x, y, z) : ts) =
  let (xs, ys, zs) = unzipTriples ts
   in (x : xs, y : ys, z : zs)

--------------------------------------------------------------------------------
-- 13. ROTATE
-- Task: Rotate a list n steps to the left
rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate 0 xs = xs
rotate n (x : xs) = rotate (n - 1) (xs ++ [x])

--------------------------------------------------------------------------------
-- 14. CONNECTED
-- Task: Check if consecutive sublists are connected (last == first of next)
connected :: (Eq a) => [[a]] -> Bool
connected [] = True
connected [_] = True
connected (x : y : zs) = last x == head y && connected (y : zs)

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- Other important functions
-- count
count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

-- foo / foo'
foo :: [(a, b, c)] -> ([a], [b], [c])
foo [] = ([], [], [])
foo ((x, y, z) : ts) = let (xs, ys, zs) = foo ts in (x : xs, y : ys, z : zs)

-- cartesian & uncartesian
cartesian :: [a] -> [b] -> [(a, b)]
cartesian [] _ = []
cartesian _ [] = []
cartesian (x : xs) ys = map (\y -> (x, y)) ys ++ cartesian xs ys

uncartesian :: (Eq a, Eq b) => [(a, b)] -> ([a], [b])
uncartesian [] = ([], [])
uncartesian ((x, y) : zs) =
  let (xs, ys) = uncartesian zs
   in (x : xs, y : ys)

-- zipIf
zipIf :: (a -> b -> Bool) -> [a] -> [b] -> [(a, b)]
zipIf p xs ys = filter (\(x, y) -> p x y) (zip xs ys)

-- split
split :: (Integral a) => [a] -> ([a], [a])
split [] = ([], [])
split (x : xs)
  | even x = (x : evens, odds)
  | otherwise = (evens, x : odds)
  where
    (evens, odds) = split xs

-- instance functions (Number type from exam)
data Number x = NaN | Value x deriving (Show)

div' :: (Eq x, Fractional x) => Number x -> Number x -> Number x
div' NaN _ = NaN
div' _ NaN = NaN
div' (Value a) (Value b) = if b == 0 then NaN else Value (a / b)

-- avg
avg :: (Fractional a) => [a] -> a
avg xs = sum xs / fromIntegral (length xs)

-- maxOnFst & maxOnFstFold
maxOnFst :: (Ord a) => [(a, b)] -> (a, b)
maxOnFst (x : xs) = mof x xs
  where
    mof max [] = max
    mof tmax@(xmax, _) (t@(x, _) : xs) = if xmax >= x then mof tmax xs else mof t xs

maxOnFstFold :: (Ord a) => [(a, b)] -> (a, b)
maxOnFstFold (x : xs) = foldl (\acc y -> if fst acc >= fst y then acc else y) x xs

-- reverse
reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []

-- find (find element satisfying predicate)
find :: (a -> Bool) -> [a] -> Maybe a
find _ [] = Nothing
find p (x : xs)
  | p x = Just x
  | otherwise = find p xs

-- depth (tree depth)
depth :: BinTree a -> Int
depth Empty = 0
depth (Leaf _) = 1
depth (Node _ l r) = 1 + max (depth l) (depth r)

-- insert into sorted list
insert :: (Ord a) => a -> [a] -> [a]
insert x [] = [x]
insert x (y : ys)
  | x <= y = x : y : ys
  | otherwise = y : insert x ys

-- wc (word/line/character count)
wc :: Char -> String -> Int
wc 'w' xs = length (filter (\a -> a == ' ' || a == '\n') xs) + 1
wc 'l' xs = length (filter (== '\n') xs) + 1
wc 'c' xs = length xs
wc _ _ = error "Char sollte w, l oder c sein"
