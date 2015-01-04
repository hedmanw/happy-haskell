import System.Random
import Data.Char
import Data.Ord
import Data.List
import Test.QuickCheck
import SuccessorArray

-- Jonathan Thunberg
-- Wilhelm Hedman
--
-- Delimiters for sentences. Could probably be different depending on input language.
sentenceSeparator = [".", "!", "?"]

-- Is a certain string at the end of a sentence?
endOfSentence :: String -> Bool
endOfSentence s = any (\sep -> sep `isSuffixOf` s) sentenceSeparator

readText :: FilePath -> IO String
readText = readFile

