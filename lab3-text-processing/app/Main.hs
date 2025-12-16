{-
   Lab: Text Processing in Haskell
   Goal: Find words that appear only in the first sentence
-}

module Main where

import Data.Char (toLower, isAlpha)
import Data.List (nub)

-- Define custom types for convenience
type Character = Char
type TextWord = String
type Sentence = String
type Delimiter = Char

-- Function to read text file
readTextFile :: FilePath -> IO String
readTextFile path = readFile path

-- Normalize whitespace: replace tabs and multiple spaces with single space
normalizeWhitespace :: String -> String
normalizeWhitespace = unwords . words

-- Check if character is sentence delimiter
isSentenceDelimiter :: Character -> Bool
isSentenceDelimiter c = c `elem` ".!?"

-- Split text into sentences
splitIntoSentences :: String -> [Sentence]
splitIntoSentences text = filter (not . null) $ map trim $ splitBySentenceDelims text
  where
    -- Split by sentence delimiters
    splitBySentenceDelims :: String -> [String]
    splitBySentenceDelims [] = []
    splitBySentenceDelims str =
      let (sentence, rest) = break isSentenceDelimiter str
      in case rest of
           [] -> [sentence]
           (_:remaining) -> sentence : splitBySentenceDelims remaining

    -- Remove spaces from beginning and end of string
    trim :: String -> String
    trim = dropWhile (== ' ') . reverse . dropWhile (== ' ') . reverse

-- Check if character is word delimiter (space or punctuation)
isWordDelimiter :: Character -> Bool
isWordDelimiter c = c == ' ' || (not (isAlpha c) && c /= '-')

-- Split sentence into words
splitIntoWords :: Sentence -> [TextWord]
splitIntoWords sentence = filter (not . null) $ map (filter isAlpha) $ splitByDelims sentence
  where
    -- Split by delimiters
    splitByDelims :: String -> [String]
    splitByDelims [] = []
    splitByDelims str =
      let (word, rest) = break isWordDelimiter str
      in case rest of
           [] -> [word]
           (_:remaining) -> word : splitByDelims remaining

-- Convert word to lowercase for comparison
normalizeWord :: TextWord -> TextWord
normalizeWord = map toLower

-- Find words that exist only in first sentence
findUniqueWordsInFirstSentence :: [Sentence] -> [TextWord]
findUniqueWordsInFirstSentence [] = []
findUniqueWordsInFirstSentence [single] = nub $ map normalizeWord $ splitIntoWords single
findUniqueWordsInFirstSentence (first:rest) =
  let firstWords = nub $ map normalizeWord $ splitIntoWords first
      otherWords = nub $ concatMap (map normalizeWord . splitIntoWords) rest
  in filter (`notElem` otherWords) firstWords

-- Main text processing function
processText :: String -> [TextWord]
processText text =
  let normalized = normalizeWhitespace text
      sentences = splitIntoSentences normalized
      uniqueWords = findUniqueWordsInFirstSentence sentences
  in uniqueWords

-- Display results
displayResults :: [TextWord] -> IO ()
displayResults words = do
  putStrLn "========================================="
  putStrLn "Words that appear only in the first sentence:"
  putStrLn "========================================="
  if null words
    then putStrLn "(No unique words)"
    else mapM_ (\w -> putStrLn $ "  - " ++ w) words
  putStrLn "========================================="

-- Main program function
main :: IO ()
main = do
  putStrLn "Reading file textbook.txt..."
  content <- readTextFile "textbook.txt"

  putStrLn "\nOriginal text:"
  putStrLn content
  putStrLn ""

  let uniqueWords = processText content
  displayResults uniqueWords
