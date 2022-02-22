-- Soren Zaiser
-- CS231, Winter 2022
-- Haskell Project #1


module DocTransformer where

import Data.List
import Data.Char

--transformDoc function needs a comment
transformDoc :: String -> Int -> String
transformDoc input desiredLength = 
    intercalate "\n" (flipCase (revList (lenCheck (wordGrab input) desiredLength)))

-- flipCase recurses over a list of strings and flips the case of each word
flipCase :: [String] -> [String]
flipCase allWords = flipCaseRec allWords [] 0

-- flipCaseRec recurses over a list of strings and flips the case of each word
flipCaseRec :: [String] -> [String] -> Int -> [String]
flipCaseRec allWords flippedWords i
    | i < len allWords =
        flipCaseRec allWords (flippedWords ++ [flipWord (allWords !! i) "" 0]) (i + 1)
    | otherwise = flippedWords

-- flipWord recursively reverses each character's case in a word
flipWord :: String -> String -> Int -> String
flipWord oldWord newWord i
    | i < len oldWord = flipWord oldWord (newWord ++ [flipLetter (oldWord !! i)]) (i + 1)
    | otherwise = newWord


--flipLetter flips a vowel to lower and consinants to upper
flipLetter :: Char -> Char
flipLetter letter
    | letter == 'A' || letter == 'E' || letter == 'I' || letter == 'O' || letter == 'U' || 
        letter == 'a' || letter == 'e' || letter == 'i' || letter == 'o' || letter == 'u' = toLower letter
    | otherwise = toUpper letter

-- revList reverses the order of all items in a list
revList :: [String] -> [String]
revList [] = []                     -- Base Case, empty list
revList (x:xs) = revList xs ++ [x]  -- Order items reversely

-- lenCheck is a helper to start lenCheckRec off correctly
lenCheck :: [String] -> Int -> [String]
lenCheck allWords desiredLength = lenCheckRec allWords [] desiredLength 0

-- lenCheckRec creates a new list only of words of a specific length
lenCheckRec :: [String] -> [String] -> Int -> Int -> [String]
lenCheckRec allWords correctWords correctLength i
    -- Check to make sure we're not at the end of the list
    | i < len allWords =
        -- Perform length check
        performlenCheckRec allWords correctWords correctLength i
    -- We're at the end of the list of words, just return all of our words now
    | otherwise = correctWords

-- performlenCheckRec checks a specific word and adds it to a list if its the correct length
performlenCheckRec :: [String] -> [String] -> Int -> Int -> [String]
performlenCheckRec allWords correctWords correctLength i
    -- Check if the length of the word is correct
    | len (allWords !! i) == correctLength =
        -- The length of the word is correct, so lets add it to our list of correct words
        lenCheckRec allWords (correctWords ++ [allWords !! i]) correctLength (i + 1)
    -- Otherwise, just call lenCheckRec again on the next word
    | otherwise = lenCheckRec allWords correctWords correctLength (i + 1)

-- wordGrab is a helper to start wordGrabRec off correctly
wordGrab :: String -> [String]
wordGrab input = wordGrabRec input [] 0 0 False


-- wordGrabRec recursises over our input string to find all words and make them into a list
wordGrabRec :: String -> [String] -> Int -> Int -> Bool -> [String]
wordGrabRec input inputList startChar currentChar inWord
    -- Check if we've reached the end of the input (Base Case)
    | currentChar == len input =
        -- Return the input list (list of our words)
        inputList
    -- Check if the current char is an alphabetical character
    | isAsciiLower (input!!currentChar) || isAsciiUpper (input!!currentChar) =
        -- Current char we're analyzing IS an alphabetical character
        isChar input inputList startChar currentChar inWord
    -- If we get here, we're neither at the end of the process or iterating over an alphabetical character
    | otherwise =
        -- We may have reached the end of a word
        notChar input inputList startChar currentChar inWord

-- isChar checks if wordGrabRec is inside of a word or not, and continues based on that
isChar :: String -> [String] -> Int -> Int -> Bool -> [String]
isChar input inputList startChar currentChar inWord
    | inWord = -- We're already in a word, just continue
        wordGrabRec input inputList startChar (currentChar + 1) True
    | otherwise = -- We're not in a word, mark inWord as true and update start counter
        wordGrabRec input inputList currentChar (currentChar + 1) True

-- notChar checks if wordGrabRec is inside of a word, and if we are, we end the word and append it to our inputList
notChar :: String -> [String] -> Int -> Int -> Bool -> [String]
notChar input inputList startChar currentChar inWord
    | inWord = -- We were in a word, now we're not. Add word to our inputList and continue iterating
        wordGrabRec input (inputList ++ [substring startChar currentChar input]) 0 (currentChar + 1) False
    | otherwise =      -- We were not in a word, just continue looking for alphabetical characters
        wordGrabRec input inputList 0 (currentChar + 1) False

-- substring gets a smaller section of a larget string
substring :: Int -> Int -> String -> String
substring start end text =
    take (end - start) (drop start text)

-- len recursively calculates the size of a list
len :: [a] -> Int
len [] = 0                  -- Base case, 0
len (x:xs) = 1 + len xs     -- Count last head, then add the rest of the list recursively