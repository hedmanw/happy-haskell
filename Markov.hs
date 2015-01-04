import System.Random
import Data.Char
import Data.Ord
import Data.List
import Test.QuickCheck
import SuccessorArray

-- Jonathan Thunberg
-- Wilhelm Hedman
--
text = "att vara eller att inte vara"
corpus = ngramOf (fromList $ words text) 2

-- Funktion som bygger ngramOfElems från corpus
-- Funktion som binärsöker fram successors
-- Funktion som bygger mening

-- Delimiters for sentences. Could probably be different depending on input language.
sentenceSeparator = [".", "!", "?"]

-- Is a certain string at the end of a sentence?
endOfSentence :: String -> Bool
endOfSentence s = any (\sep -> sep `isSuffixOf` s) sentenceSeparator

readText :: FilePath -> IO String
readText = readFile

