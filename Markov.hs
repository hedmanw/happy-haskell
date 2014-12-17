import System.Random
import Data.Char
import Data.Ord
import Data.List
import Test.QuickCheck
import Ngram

-- Markov generator
-- in haskell!
-- Jonathan Thunberg
-- Wilhelm Hedman
-- 
-- We have implemented 2/3 of our goals. We dropped the idea
-- that you should be able to have a "conversation".
-- Instead, the program now generates a sentence based on
-- an input text.
-- The provided text file, slow_loris.txt is taken from the
-- Wikipedia page for Slow Loris. It can be used as input
-- for printMarkovText.

-- Delimiters for sentences. Could probably be different depending on input language.
sentenceSeparator = [".", "!", "?"]

-- Is a certain string at the end of a sentence?
endOfSentence :: String -> Bool
endOfSentence s = any (\sep -> sep `isSuffixOf` s) sentenceSeparator

-- Chooses a whole sentence, word by word, given a starting word for the sentence.
getSentence :: Dict -> String -> StdGen -> String
getSentence dict start gen = unwords $ reverse $ buildSentence dict [start] gen
  where buildSentence :: Dict -> [String] -> StdGen -> [String]
        buildSentence dict sen gen
         | fst (getSuccessor dict [head sen] gen) == "" = sen
         | otherwise = 
           let succ = getSuccessor dict [head sen] gen in
           buildSentence dict (fst succ : sen) (snd succ)

-- Prints a generated sentence based on the input file.
printMarkovText :: FilePath -> IO ()
printMarkovText f = do
               gen <- newStdGen
               text <- readText f
               let list = wordOrder (Ngram 1) (wordList text)
               let upperList = filter (isAsciiUpper . head . head . fst) list
               let (seed, nextGen) = randomR (0, length upperList-1) gen
               putStrLn $ getSentence list (head $ fst $ upperList !! seed) nextGen

readText :: FilePath -> IO String
readText = readFile

