-- Soren Zaiser
-- CS231, Winter 2022
-- Haskell Lab #2: wrapInsert

-- Main wrapInsert function that starts the recursion
wrapInsert :: a -> [a] -> [a]
wrapInsert toInsert list = wrapInsertRecursive toInsert (len list) list -- Start recursion

-- Recursively reverse-iterates over a list an inserts 'toInsert' at each location
wrapInsertRecursive :: a -> Integer -> [a] -> [a]
wrapInsertRecursive toInsert insertLocation list
 -- InsertLocation is no negative (IE a valid list position)
 | insertLocation - 1 > -1 = 
     wrapInsertRecursive                              -- Recursively call this method again
        toInsert                                      -- Character to insert 
        (insertLocation - 1)                          -- Next location to insertAt
        (insertAt toInsert insertLocation list)       -- Insert Character at our desired location

 -- Base Case, no more recursive iterations. This inserts at character 0       
 | otherwise = insertAt toInsert insertLocation list  


-- Recursively insert item into a list at a specific location
insertAt :: a -> Integer -> [a] -> [a]
insertAt newElement 0 list = newElement:list                        -- Base case, we're at the position we want to insert the element, insert it
insertAt newElement i (x:xs) = x : insertAt newElement (i - 1) xs   -- Recursively iterate over list until we get to the position we want to be at

-- Recursively count items in a list (calcualte length of list)
len :: [a] -> Integer
len [] = 0                  -- Base case, 0
len (x:xs) = 1 + len xs     -- Count last head, then add the rest of the list recursively