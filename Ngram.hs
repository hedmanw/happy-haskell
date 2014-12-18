module Ngram where

import qualified Data.Vector as V
import System.Random
import Test.QuickCheck
-- TDA452
--

list = wordOrder (Ngram 1) ["a", "b", "c", "d"]

data Ngram = Ngram Int
  deriving Show

type Dict = [([String],String)]

data SuffixArray a = SuffixArray (V.Vector a) (V.Vector Int)
  deriving Show


wordList :: String -> [String]
wordList = words

wordOrder :: Ngram -> [String] -> Dict
wordOrder _ [] = []
wordOrder (Ngram n) ws
  | length ws > n = (take n ws,ws !! n) : wordOrder (Ngram n) (drop 1 ws)
  | length ws == n = [(ws, "")]

prop_wordOrder :: Ngram -> String -> Property
prop_wordOrder (Ngram n) s =
    n <= length ws ==> (1+length ws-n) == (length (wordOrder (Ngram n) ws))
  where ws = wordList s

getSuccessor :: Dict -> [String] -> StdGen -> (String,StdGen)
getSuccessor dict words = chooseWord (filter (\w -> fst w == words) dict)

chooseWord :: Dict -> StdGen -> (String,StdGen)
chooseWord []   gen = ("",gen)
chooseWord dict gen = (snd (dict !! index),nextGen)
  where (index, nextGen) = randomR (0, length dict-1) gen

instance Arbitrary Ngram where
  arbitrary = do n <- choose (1,4)
                 return (Ngram n)

data RandomDict = Rnd Dict
  deriving Show

instance Arbitrary RandomDict where
  arbitrary = do
    words <- listOf1 $ listOf1 $ elements ['a'..'z']
    n <- choose (1, length words)
    return (Rnd (wordOrder (Ngram n) words))

instance Arbitrary StdGen where
  arbitrary = do n <- arbitrary
                 return (mkStdGen n)

