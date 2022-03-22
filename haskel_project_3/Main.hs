-- Soren Zaiser
-- CS231, Winter 2022
-- Haskell Project #2

module Main where
import System.Environment
import System.Directory
import System.IO

import Debug.Trace

-- Temp Dict File Name
tempDictFileName = "tmpDictionary.tmp"

-- Temp Prime File Name
tempPrimeFileName = "tmpPrime.tmp"

main :: IO()
main = do
    -- Print breif intro to kill time while loading program
    printHelp
    -- Get program command line arguments
    [dictFile] <- getArgs
    -- Check if temporary dictionary exists on filesystem
    tempDictExists <- doesFileExist tempDictFileName
    -- Check if temporary prime exists on filesystem
    tempPrimeExists <- doesFileExist tempPrimeFileName
    -- Check if the temporary dictionary exists
    if not tempDictExists then
        -- Print Helpful message as to why program may hang for a second
        readFile dictFile
        -- Write 5 letter dictionary to file
        >>= \temp ->  writeFile tempDictFileName (wordFormatter (dictCreator temp))
    else
        putStr ""

    if not tempPrimeExists then
        -- Get number between 0 and 10
        getRange 11
        -- Write math to file
        >>= \t ->  writeFile tempPrimeFileName (show (pseudoRandom (toInteger t)))
    else
        putStr ""

    -- Read Temp Dictionary
    tempDict <- readFile tempDictFileName
    -- Read Temp Prime from file
    tempPrime <- readFile tempPrimeFileName
    -- Read the full dict from file
    tempFullDict <- readFile dictFile

    -- Start wordle game
    startWordle (lines tempDict) (wordSelector (length tempDict) (read tempPrime)) (dictCreator tempFullDict)

-- printHelp prints the help text
printHelp :: IO()
printHelp =
        putStrLn "\nWelcome to fermi-pico-nano-wordle. The goal is to guess a random 5-letter word within 8 guesses.\n\n\
             \After each guess, a 5-digit score representing your word accuracy will be printed:\n\
             \      3 - Correct letter, correct location\n\
             \      7 - Letter exists, but is in the wrong location\n\
             \      9 - Letter does not exist in word\n\
             \Good luck!\n\n\
             \Type :? for help at any time\n"

-- startWordle starts the wordle game
startWordle :: [String] -> Integer -> [String] -> IO()
startWordle dict wordNum fullDict = wordleRec dict fullDict (dict !! (fromInteger wordNum)) 0 wordNum

-- wordSelector selects the word based on the psudo random number and the size of the dictionary
wordSelector :: Int -> Int -> Integer
wordSelector dictSize pRand = toInteger (pRand `mod` dictSize)

-- worldRec recursively plays wordle!
wordleRec :: [String] -> [String] -> String -> Integer -> Integer -> IO()
wordleRec dict fullDict word guesses dictPos
    | guesses > 7 =
        putStrLn ("You've exceeded the number of allowed guesses. The word was '" ++ word ++ "'. Better luck next time!")
        >> postGame dict dictPos
    | otherwise =
        -- putStrLn word -- spoiler!
        getWordle fullDict "" guesses True
         >>= \guess ->  processWord word guess 0 guesses dict dictPos fullDict ""

-- processWord recursively processes a wordle word
processWord :: String -> String -> Int -> Integer -> [String] -> Integer -> [String] -> String -> IO()
processWord word guess guessPos numGuesses dict dictPos fullDict guessAccuracy
    | guessPos > 4 =                                                                -- We're at the end of the word, call next guess
        putStrLn guessAccuracy
        >> wordleRec dict fullDict word (numGuesses + 1) dictPos
    | word == guess =                                                               -- Guess is correct, end game
        putStrLn "33333\nCongrats, you guessed the correct word!"                   -- Print game-ending prompt
        >> postGame dict dictPos                                                    -- Remove word from dictionary
    | (word !! guessPos) == (guess !! guessPos) =                                   -- Current character is correct (match, 3)
        -- Append 3, character is correct and in correct spot then recurse over next character
        putStr ""
        >> processWord word guess (guessPos + 1) numGuesses dict dictPos fullDict (updateGuessAccuracy '3' guessAccuracy)
    | contains word (guess !! guessPos) 0 =                                         -- Wordle contains the current character
        -- Append 7, character exists but in wrong spot then recurse over next character
        putStr ""
        >> processWord word guess (guessPos + 1) numGuesses dict dictPos fullDict (updateGuessAccuracy '7' guessAccuracy)
    | otherwise =                                                                   -- Wordle does not contain current character
        -- Append 9, incorrect character then recurse over next character
        putStr ""
        >> processWord word guess (guessPos + 1) numGuesses dict dictPos fullDict (updateGuessAccuracy '9' guessAccuracy)

-- updateGuessAccuracy updates the accuracy string of a wordle word
updateGuessAccuracy :: Char -> String -> String
updateGuessAccuracy newChar currentAcc
    | newChar == '3' = newChar : currentAcc
    | newChar == '9' = currentAcc ++ [newChar]
    | newChar == '7' = insertSeven currentAcc 0

-- insertSeven inserts a 7 after the last 3
insertSeven :: String -> Int -> String
insertSeven currentAcc position
    | (length currentAcc) == position = currentAcc ++ ['7']                     -- We're at the end of the current accuracy string, just insert the 7
    | (currentAcc !! position) == '3' = insertSeven currentAcc (position + 1)   -- Found a 3, keep going
    | otherwise = insertAt '7' position currentAcc                              -- No 3 found, safe to insert a 7

-- Insert At inserts a specific element at a specific location in a list
insertAt :: a -> Int -> [a] -> [a]
insertAt newElement 0 as = newElement:as
insertAt newElement i (a:as) = a : insertAt newElement (i - 1) as

-- postGame removes the used word and writes the new temp dictionary
postGame :: [String] -> Integer -> IO()
postGame dict dictPos =
    -- Write 5 letter dictionary to file
    writeFile tempDictFileName (wordFormatter (removeWord (fromInteger dictPos) dict))

-- removeWord removes a word from the dictionary
removeWord :: Int -> [String] -> [String]
removeWord toRemove dict = take toRemove dict ++ drop (1 + toRemove) dict

-- contains checks if a string contains a character
contains :: String -> Char -> Int -> Bool
contains word character position
    | position == length word = False                   -- We're at the end of the word, no match found
    | (word !! position) == character = True            -- We found a match
    | otherwise = contains word character (position + 1)-- no match found yet, not at the end of a word, keep going

-- pseudoRandom performs Prof. Vineyard's funky math equation on a number
pseudoRandom :: Integer -> Integer
pseudoRandom n = (n * (prime 1101) `mod` 1103)+7

-- primes produces a list of infinite primes
primes :: [Integer]
primes = sieve [2..] --infinite list, 2 is in position 0

-- sieve sorts through an infinite list to find primes
sieve :: [Integer] -> [Integer]
sieve (x:xs) = x : sieve [y | y <- xs, mod y x /= 0] -- some elements
                                  -- removed at each step, still infinite

-- prime selectes the nth prime number
prime :: Integer -> Integer
prime n = primes !! ((fromInteger n) - 1) -- counting starts at 0

-- dictCreator is a function to filter the larger dict into the 5 letter temporary dictionary
dictCreator :: String -> [String]
dictCreator inputDict = filter (\x -> length x == 5) (lines inputDict)

-- dictExists checks if a word exists in the dictionary
dictExists :: [String] -> String -> Bool
dictExists dict word = (length (filter (\x -> x == word) dict)) > 0

-- wordFormatter is a helper function to start wordFormatterRec correctly
wordFormatter :: [String] -> String
wordFormatter allWords = wordFormatterRec allWords 0 (length allWords)

-- readInt gets a line from the console and reads it as an int
readInt :: IO Int
readInt = readLn    -- Get line fron console

-- getNumberFromConsole prints a header followed by reading an int from the console
getNumberFromConsole :: IO Int
getNumberFromConsole =
    putStrLn "Enter Number between 1 and 10: " -- Print prompt
    >> readInt                                 -- Read number from console

-- getStringFromConsole prints a header followed by reading a string from the console
getStringFromConsole :: String -> IO [Char]
getStringFromConsole prompt =
    putStrLn prompt              -- Print prompt
    >> getLine                    -- Read string from console

-- getWordle gets a world word from the console, ensuring it's not to long or short
getWordle :: [String] -> String -> Integer -> Bool -> IO String
getWordle dict wordle guesses first
    | wordle == ":?" = 
        printHelp
        >> getWordle dict "" guesses True
    | length wordle == 5 && (dictExists dict wordle) =                          -- Word is 5 letters long, and exists in the dictionary
        return wordle
    | first =                                                                   -- This is the first input, ignore invalidness
        getStringFromConsole ("Enter Guess #" ++ (show (guesses + 1)) ++ ": ")  -- Get word from console
        >>= \t ->  getWordle dict t guesses False                               -- Recurse over this function to process word
    | otherwise =                                                               -- Non-dictionary word, or wrong length word
        putStrLn "Invalid guess. Worlde words must contain only 5 letters, and must be a valid word"
            >> getStringFromConsole ("Enter Guess #" ++ (show (guesses + 1)) ++ ": ")
            >>= \t ->  getWordle dict t guesses False                           -- Recurse over this function again to get a new input


-- getRange gets a number between 1 and 10, and only that
getRange :: Int -> IO Int
getRange consoleInt
    | consoleInt > 0 && consoleInt < 11 = -- Number is correct, between 1 and 10
        return consoleInt
    | otherwise =                         -- Number is incorrect, query console again
        getNumberFromConsole
        >>= \t ->  getRange t

-- wordFormatterRec formats the list of incorrect words into a printable string
wordFormatterRec :: [String] -> Int -> Int -> String
wordFormatterRec allWords currentWord allWordsSize
         -- Base case, no more words
     | currentWord == allWordsSize = ""
         -- Recurse over the words and pretty print them
     | otherwise = (wordFormatterRec allWords (currentWord + 1) allWordsSize) ++ (allWords !! currentWord) ++ "\n"