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
    -- Get program command line arguments
    [dictFile] <- getArgs
    -- Check if temporary dictionary exists on filesystem
    tempDictExists <- doesFileExist tempDictFileName
    -- Check if temporary prime exists on filesystem
    tempPrimeExists <- doesFileExist tempPrimeFileName
    -- Check if the temporary dictionary exists
    if not tempDictExists then
        -- Print Helpful message as to why program may hang for a second
        --putStrLn "STARTUP : Temporary dictionary unavaliable, createing it now! This may take a second..."
        readFile dictFile
        -- Write 5 letter dictionary to file
        >>= \temp ->  writeFile tempDictFileName (wordFormatter (dictCreator temp))
    else
        -- putStrLn "STARTUP : Temporary dictionary avaliable. Loading it..."
        putStr ""

    if not tempPrimeExists then
        -- putStrLn "STARTUP : No temporary prime"
        -- Get number between 0 and 10
        getRange 11
        -- Write math to file
        >>= \t ->  writeFile tempPrimeFileName (show (pseudoRandom (toInteger t)))
    else
        -- putStrLn "STARTUP : Temporary prime exists. Loading it..."
        putStr ""

    -- Read Temp Dictionary
    tempDict <- readFile tempDictFileName
    -- Read Temp Prime from file
    tempPrime <- readFile tempPrimeFileName

    -- Print breif intro (pfft \n is overrated anyway)
    putStrLn "Welcome to wordle. The goal is to guess a random 5-letter word within 8 guesses.\n"
    putStrLn "After each guess, a 5-digit score representing your word accuracy will be printed:"
    putStrLn "      3 - Correct letter, correct location"
    putStrLn "      7 - Letter exists, but is in the wrong location"
    putStrLn "      9 - Letter does not exist in word\n"
    putStrLn "Good luck!\n"

    -- Start wordle game
    startWordle (lines tempDict) (wordSelector (length tempDict) (read tempPrime))

-- startWordle starts the wordle game
startWordle :: [String] -> Integer -> IO()
startWordle dict wordNum = wordleRec dict (dict !! (fromInteger wordNum)) 0 wordNum

-- wordSelector selects the word based on the psudo random number and the size of the dictionary
wordSelector :: Int -> Int -> Integer
wordSelector dictSize pRand = toInteger (pRand `mod` dictSize)

-- worldRec recursively plays wordle!
wordleRec :: [String] -> String -> Integer -> Integer -> IO()
wordleRec dict word guesses dictPos
    | guesses > 7 =
        putStrLn ("You've exceeded the number of allowed guesses. The word was '" ++ word ++ "'. Better luck next time!")
        >> postGame dict dictPos
    | otherwise =
        -- putStrLn word -- spoiler!
        getWordle dict "" guesses True
         >>= \guess ->  processWord word guess 0 guesses dict dictPos

-- processWord recursively processes a wordle word
processWord :: String -> String -> Int -> Integer -> [String] -> Integer -> IO()
processWord word guess position guesses dict dictPos
    | position > 4 =                                                    -- We're at the end of the word, call next guess
        putStrLn ""
        >> wordleRec dict word (guesses + 1) dictPos
    | word == guess =                                                   -- Guess is correct, end game
        putStrLn "33333\nCongrats, you guessed the correct word!"       -- Print game-ending prompt
        >> postGame dict dictPos                                        -- Remove word from dictionary
    | (word !! position) == (guess !! position) =                       -- Current character is correct (match, 3)
        putStr "3"                                                      -- Print 3, character is correct and in correct spot
        >> processWord word guess (position + 1) guesses dict dictPos   -- Recurse over next character
    | contains word (guess !! position) 0 =                             -- Wordle contains the current character
        putStr "7"                                                      -- Print 7, character exists but in wrong spot
        >> processWord word guess (position + 1) guesses dict dictPos   -- Recurse over next character
    | otherwise =                                                       -- Wordle does not contain current character
        putStr "9"                                                      -- Print 9, incorrect character
        >> processWord word guess (position + 1) guesses dict dictPos   -- Recurse over next character

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