-- Soren Zaiser
-- CS231, Winter 2022
-- Haskell Project #2

module Main where
import System.Environment
import System.IO
import Data.Char
import Data.Text hiding (take, drop, lines, head)
import Data.Time

import Debug.Trace

{-
main is the entry point to the program
then run the program with command
     ./Main in.txt dictionary.txt out.txt
where in.txt is the name of the input file, out.txt is the name of the
output file, and dictionary.txt is the name of the dictionary file
-}

-- Spelling State Enum (For tracking status of a word)
data SpellingState = Correct | Incorrect deriving (Enum,Show,Eq)
-- Word Datatype, used to track data about the location and spelling of a word
data FullWord = FullWord {
                              fullWord :: String,
                              lowerWord :: String,
                              line :: Int,
                              col :: Int,
                              state :: SpellingState
                         } deriving (Eq, Show)
main :: IO()
main = do
  -- Record Start Time
  start <- getCurrentTime
  [inFile, dictFile, outFile] <- getArgs
  input <- readFile inFile
  dict <- readFile dictFile
  let output = wordFormatter (dictIterator (lines dict) (lineIterator input))
  writeFile outFile output
  -- Print total time elapsed to do this intense calculation
  end <- getCurrentTime
  putStrLn "Total Time Elapsed: "
  print (diffUTCTime end start)

-- dictIterator is a helper method to setup dictIteratorRec correctlty
dictIterator :: [String] -> [FullWord] -> [FullWord]
dictIterator dict allWords = dictIteratorRec dict 0 allWords 0 (len dict) (len allWords)	-- we calculate the sizes here, once, so we're not calculating it every recursion

-- dictIteratorRec recursivelty iterates over the provided dictionary and the provided word list and marks correctly spelled words
dictIteratorRec :: [String] -> Int -> [FullWord] -> Int -> Int -> Int -> [FullWord]
dictIteratorRec dict currentDict allWords currentWord dictSize allWordsSize
     -- We're at the end of allWords, base case
     | currentWord == allWordsSize = []
     -- We're at the end of the dictionary
     | currentDict == dictSize = []
     -- Our current word is still of higher alphabitization than the dict word, move to next dict word
     | lowerWord (allWords!!currentWord) > dict!!currentDict =
          dictIteratorRec dict (currentDict + 1) allWords currentWord dictSize allWordsSize
     -- Our current word equal to our current dict, and is spelled correctly
     | lowerWord (allWords!!currentWord) == dict!!currentDict =
          (dictIteratorRec dict currentDict allWords (currentWord + 1) dictSize allWordsSize) ++ [markCorrect (allWords!!currentWord)]
     -- We've alphabetically passed the current word in the dictionary, it's spelled wrong
     | lowerWord (allWords!!currentWord) < dict!!currentDict =
          (dictIteratorRec dict currentDict allWords (currentWord + 1) dictSize allWordsSize) ++ [allWords!!currentWord]

-- markCorrect marks a FullWord as correct by regenerating the DataType with the Correct enum
markCorrect :: FullWord -> FullWord
markCorrect old = FullWord (fullWord old) (lowerWord old) (line old) (col old) Correct

-- wordFormatter is a helper function to start wordFormatterRec correctly
wordFormatter :: [FullWord] -> String
wordFormatter allWords = wordFormatterRec allWords 0

-- wordFormatterRec formats the list of incorrect words into a printable string
wordFormatterRec :: [FullWord] -> Int -> String
wordFormatterRec allWords currentWord
	 -- Base case, no more words
     | currentWord == len allWords = ""
	 -- Recurse over the words and pretty print them
     | otherwise = wordFormatterRec allWords (currentWord + 1) ++ formatWord (allWords!!currentWord)

-- formatWord pretty-prints a FullWord object
formatWord :: FullWord -> String
formatWord word = "The word '" ++ fullWord word ++ "' was found at " ++ show ((line word) + 1) ++ ":" ++ show ((col word) + 1) ++ " and the spelling is " ++ show (state word) ++ "\n"

-- lineIterator sets up lineIteratorRec to run correctly
lineIterator :: String -> [FullWord]
lineIterator input = lineIteratorRec (lines input) 0 []

-- lineIteratorRec iterates over the lines of our input file and get the coordinates of our words
lineIteratorRec :: [String] -> Int -> [FullWord] -> [FullWord]
lineIteratorRec allLines currentLine allWords
     -- Base case, end of lines, return allWords
     | currentLine == len allLines = allWords
     -- We still have lines left to iterate, recurse over them
     | otherwise = wordGrab (allLines!!currentLine) currentLine (lineIteratorRec allLines (currentLine + 1) allWords)


-- wordGrab is a helper to start wordGrabRec off correctly
wordGrab :: String -> Int -> [FullWord] -> [FullWord]
wordGrab input lineNumber allWords = wordGrabRec input allWords lineNumber 0 0 False


-- wordGrabRec recursises over our input file string to find all words and make them into a list
wordGrabRec :: String -> [FullWord] -> Int -> Int -> Int -> Bool -> [FullWord]
wordGrabRec input allWords currentLine startChar currentChar inWord
    -- Check if we've reached the end of the input (Base Case)
    | currentChar == len input =
        -- Return the input list (list of our words)
        allWords
    -- Check if the current char is an alphabetical character
    | isAsciiLower (input!!currentChar) || isAsciiUpper (input!!currentChar) =
        -- Current char we're analyzing IS an alphabetical character
        isChar input allWords currentLine startChar currentChar inWord
    -- If we get here, we're neither at the end of the process or iterating over an alphabetical character
    | otherwise =
        -- We may have reached the end of a word
        notChar input allWords currentLine startChar currentChar inWord


-- isChar checks if wordGrabRec is inside of a word or not, and continues based on that
isChar :: String -> [FullWord] -> Int -> Int -> Int -> Bool -> [FullWord]
isChar input allWords lineNumber startChar currentChar inWord
    | inWord = -- We're already in a word, just continue
        wordGrabRec input allWords lineNumber startChar (currentChar + 1) True
    | otherwise = -- We're not in a word, mark inWord as true and update start counter
        wordGrabRec input allWords lineNumber currentChar (currentChar + 1) True

-- notChar checks if wordGrabRec is inside of a word, and if we are, we end the word and append it to our inputList
notChar :: String -> [FullWord] -> Int -> Int -> Int -> Bool -> [FullWord]
notChar input allWords lineNumber startChar currentChar inWord
    | inWord = -- We were in a word, now we're not. Add word to our inputList and continue iterating
        wordGrabRec input
               (insertAlphabetically (FullWord (substring startChar currentChar input) (lwr (substring startChar currentChar input)) lineNumber startChar Incorrect) allWords)
               lineNumber          -- Pass along line number
               0                   -- Set word starting position back to 0
               (currentChar + 1)   -- Iterate over next character
               False               -- Set inWord to false, as we're no longer inside of a word
    | otherwise =      -- We were not in a word, just continue looking for alphabetical characters
        wordGrabRec input allWords lineNumber 0 (currentChar + 1) False


-- insertAlphabetically is a helper function to start insertAlphabeticallRec correctly
insertAlphabetically :: FullWord -> [FullWord] -> [FullWord]
insertAlphabetically newWord allWords = insertAlphabeticallyRec newWord allWords 0

-- insertAlphabeticallyRec recursively iterates over a list of FullWords to insert a word alphabetically
insertAlphabeticallyRec :: FullWord -> [FullWord] -> Int -> [FullWord]
insertAlphabeticallyRec newWord allWords currentPosition
     -- We've reached the end of the list, append word to end of list
     | currentPosition == len allWords = allWords ++ [newWord]
     | (lowerWord newWord) < (lowerWord (allWords!!currentPosition)) = insertAt newWord currentPosition allWords
     | otherwise = insertAlphabeticallyRec newWord allWords (currentPosition + 1)


-------- Generic Functions that do Generic Things ------------

-- insertAt Recursively insert item into a list at a specific location
insertAt :: a -> Int -> [a] -> [a]
insertAt newElement 0 list = newElement:list                        -- Base case, we're at the position we want to insert the element, insert it
insertAt newElement i (x:xs) = x : insertAt newElement (i - 1) xs   -- Recursively iterate over list until we get to the position we want to be at


-- substring gets a smaller section of a larget string
substring :: Int -> Int -> String -> String
substring start end text =
    take (end - start) (drop start text)

-- lwr is shorthand to quickly lowercase a string value and keep it a string
lwr :: String -> String
lwr txt = unpack (Data.Text.toLower (pack txt))

-- len recursively calculates the size of a list
len :: [a] -> Int
len [] = 0                  -- Base case, 0
len (x:xs) = 1 + len xs     -- Count last head, then add the rest of the list recursively