main :: IO ()
main =
  putStrLn "How many numbers would you like to add? "
    >>  readInt
    >>= \count -> addFromConsole count
    >>= \output -> print output

getNumberFromConsole :: IO Int
getNumberFromConsole =
    putStrLn "Enter Number: "
    >> readInt

addFromConsole :: Int -> IO Int
addFromConsole 0 = return 0
addFromConsole n =
  addFromConsole (n - 1)
    >>= \recurs ->
        getNumberFromConsole
            >>= \input -> return (input + recurs)



-- addFromConsole (remainingToRead - 1) >>= \recur ->
--             putStrLn "test"
--             >> getNumberFromConsole >>= \num -> 
--                 return num + recur


    -- | remainingToRead >= 0 = 
    --     addFromConsole (remainingToRead - 1) (putStrLn "Enter Number" >> (readInt) (currentValue) >>= \num cv -> num + cv)
    -- | otherwise = show currentValue

readInt :: IO Int
readInt = readLn