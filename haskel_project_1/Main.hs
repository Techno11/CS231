-- Soren Zaiser
-- CS231, Winter 2022
-- Haskell Project #1
-- This file unmodified, and written by Professor Vineyard

{-
This code will call your function 'transformDoc' in the file
   DocTransformer.hs
This code will be run with 3 command line arguments and will
handle all input and output.  Your transformDoc function will
do the task described in the requirements document for
project 1 in Haskell.
-}

module Main where
import DocTransformer
import System.Environment
import System.IO

{-
main is the entry point to the program.  Compile the code with command
     ghc --make Main.hs
then run the program with command
     ./Main in.txt out.txt num
where in.txt is the name of the input file, out.txt is the name of the
output file, and num will be replaced by an integer token, such as 17.
-}

main = do
  [inFile, outFile, length] <- getArgs
  input <- readFile inFile
  let size = read length :: Int
  let output = transformDoc input size
  writeFile outFile output