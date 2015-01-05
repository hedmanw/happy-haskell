import System.Random
import Data.Maybe
import Data.Char
import Data.Ord
import Data.List
import Test.QuickCheck
import SuccessorArray
import qualified Data.Vector as V

-- Jonathan Thunberg
-- Wilhelm Hedman
--
text = "att vara eller att inte vara"
corpus = ngramOf (fromList $ words text) 2

-- Funktion som bygger ngramOfElems från corpus
buildNgram :: Ord a => [a] -> Int -> V.Vector (V.Vector a)
buildNgram cs n = ngramFromElems (fromList cs) n

type Successors = V.Vector (V.Vector String)

buildMarkovNgram :: [String] -> Int -> Successors
buildMarkovNgram = buildNgram

buildOptimalMarkovNgram :: String -> Successors
buildOptimalMarkovNgram inputText = buildMarkovNgram (words inputText) 2

-- Funktion som binärsöker fram successors
getSuccessor :: Successors -> String -> StdGen -> (String, StdGen)
getSuccessor sa w gen
  | isNothing successor = ("",gen)
  | otherwise = (V.last $ sa V.! rand, nextGen)
  where successor = binarySearch search (0, V.length sa -1) 
        search = searchSuccessor sa w
        (lower, upper) = getAllSuccessors search sa (fromJust successor)
        (rand, nextGen) = randomR (lower, upper) gen

searchSuccessor sa w n = w `compare` (V.head $ sa V.! n)

getAllSuccessors :: (Int -> Ordering) -> Successors -> Int -> (Int, Int)
getAllSuccessors search sa n = (lowerIndex, upperIndex)
  where lowerIndex = fromJust $ lowerIndexOfBy search (0, n)
        upperIndex = fromJust $ upperIndexOfBy search (n, V.length sa -1)

-- Funktion som bygger mening
getSentence :: Successors -> String -> StdGen -> String
getSentence sa start gen =unwords $ getRecSentence sa start gen

getRecSentence :: Successors -> String -> StdGen -> [String]
getRecSentence sa w gen |endOfSentence w = [w]  
                        |otherwise = w : getRecSentence sa word nextGen
  where (word, nextGen) = getSuccessor sa w gen

-- Delimiters for sentences. Could probably be different depending on input language.
sentenceSeparator = [".", "!", "?"]

-- Is a certain string at the end of a sentence?
endOfSentence :: String -> Bool
endOfSentence s = any (\sep -> sep `isSuffixOf` s) sentenceSeparator

readText :: FilePath -> IO String
readText = readFile

printMarkovText :: FilePath -> IO()
printMarkovText f = do 
    gen <- newStdGen
    
    text <- readText f
    let ngrams = buildOptimalMarkovNgram text
    let startIndex =fromJust $ binarySearch (findCaps ngrams) (0, V.length ngrams - 1)
    let (first, nextGen) = randomR (getAllSuccessors (findCaps ngrams) ngrams startIndex) gen 
    putStrLn $ getSentence ngrams (V.head $ ngrams V.! first) nextGen

findCaps :: Successors -> Int -> Ordering
findCaps sa n | ascii < 65 = LT
              | ascii > 90 = GT
              | otherwise  = EQ
  where ascii = ord $ head $ V.head $ sa V.! n

