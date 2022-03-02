main :: IO ()
main =
  putStrLn "How many numbers would you like to add? "
    >>  readInt
    >>= \count -> addFromConsole count
    >>= \output -> 
        putStrLn "Your Sum: "
        >> print output

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


readInt :: IO Int
readInt = readLn