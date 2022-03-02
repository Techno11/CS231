main :: IO ()
main =
  putStrLn "How many numbers would you like to add? " -- Print initial prompt
    >>  readInt                             -- Read Initial Int from Console to determine count
    >>= \count -> addFromConsole count      -- Start recursive adder
    >>= \output ->                          -- Get output from IO
        putStrLn "Your Sum: "               -- Make output look good
        >> print output                     -- Print final output

-- getNumberFromConsole prints a header followed by reading an int from the console
getNumberFromConsole :: IO Int
getNumberFromConsole = 
    putStrLn "Enter Number: "               -- Print "Enter Number: "
    >> readInt                              -- Read number from console

-- addFromConsole recursively adds integers from the console
addFromConsole :: Int -> IO Int
addFromConsole 0 = return 0             -- Base Case
addFromConsole n =                      
  addFromConsole (n - 1)                -- Recursively call addFromConsole, decrementing our remaining count
    >>= \recurs ->                      -- Get output from recursive call
        getNumberFromConsole            -- Get the next number to add from the console
            >>= \input -> return (input + recurs)   -- Add the recursive output and the input from the console


-- readInt gets a line from the console and reads it as an int
readInt :: IO Int
readInt = readLn    -- Get line fron console