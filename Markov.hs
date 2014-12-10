import System.Random
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
wordList = words

wordOrder2 :: [String] -> [(String,String)]
wordOrder2 (x:[]) = []
wordOrder2 (f:x:xs) = (f,x) : wordOrder2 (x:xs)

sortByWord :: [(String,String)] -> [(String,String)]
sortByWord = sortBy (comparing fst)

sortedList = sortByWord $ wordOrder2 $ wordList text

getSuccessor :: [(String,String)] -> String -> StdGen -> String
getSuccessor dict word stdgen = choose (filter (\w -> fst w == word) dict) stdgen
  where choose :: [(String, String)] -> StdGen -> String
        choose dict gen = snd (dict !! fst (randomR (0, length dict-1) gen))

doEverything :: IO ()
doEverything = do
               gen <- newStdGen
               putStrLn $ getSuccessor sortedList "the" gen

