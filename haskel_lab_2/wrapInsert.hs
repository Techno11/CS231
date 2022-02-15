-- Soren Zaiser
-- CS231, Winter 2022
-- Haskell Lab #2: wrapInsert


wrapInsert :: a -> [a] -> [a]
wrapInsert toInsert list = wrapInsertRecursive toInsert (len list) list

wrapInsertRecursive :: a -> Integer -> [a] -> [a]
wrapInsertRecursive toInsert insertLocation list
 | insertLocation - 1 > -1 = do
     let newList = insertAt toInsert insertLocation list
     wrapInsertRecursive toInsert (insertLocation - 1) newList
 | otherwise = insertAt toInsert insertLocation list


-- Insert an item into a list using recursion
insertAt :: a -> Integer -> [a] -> [a]
insertAt newElement 0 as = newElement:as
insertAt newElement i (a:as) = a : insertAt newElement (i - 1) as

-- Count heads (calcualte length of list)
len :: [a] -> Integer
len [] = 0
len (x:xs) = 1 + len xs