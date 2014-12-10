import Data.Ord
import Data.List
-- Markov generator
-- in haskell!
-- Jonathan Thunberg
-- Wilhelm Hedman
--

text :: String
text = "of the people, by the people, for the people"

wordList :: String -> [String]
wordList str = words str

wordOrder2 :: [String] -> [(String,String)]
wordOrder2 (x:[]) = []
wordOrder2 (f:x:xs) = (f,x) : wordOrder2 (x:xs)

sortByWord :: [(String,String)] -> [(String,String)]
sortByWord = sortBy (comparing fst)
