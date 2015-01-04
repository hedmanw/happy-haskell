module SuccessorArray where

import Test.QuickCheck
import Data.Maybe
import qualified Data.Vector as V
import qualified Data.List as L

-- SuccessorArray contains one vector, containing the corpus, and one vector
-- which represents the natural order of the items in the corpus.
data SuccessorArray a = SuccessorArray (V.Vector a) (V.Vector Int)
  deriving Show

instance (Arbitrary a, Ord a) => Arbitrary (SuccessorArray a) where
  arbitrary = do
    list <- arbitrary
    return (fromList list)

-- Compare subvectors at indices i and j of corpus c.
saCompare :: Ord a => V.Vector a -> Int -> Int -> Ordering
saCompare c i j = compare (V.drop i c) (V.drop j c)

-- Construct a SuccessorArray from a single vector containing the corpus
suffixArray :: Ord a => V.Vector a -> SuccessorArray a
suffixArray c = SuccessorArray c (V.fromList wordIndices)
  where corpusSize = V.length c -1
        allIndices = [0..corpusSize]
        wordIndices = L.sortBy (saCompare c) allIndices

-- Make a SuccessorArray from a List
fromList :: Ord a => [a] -> SuccessorArray a
fromList = suffixArray . V.fromList

-- Make a list of lists containing each suffix sequence, in order of the
-- first item of the suffix sequence.
toList :: SuccessorArray a -> [[a]]
toList (SuccessorArray c i) = V.foldr vectorAt [] i
  where vectorAt index l = V.toList (V.drop index c) : l

-- Get all n-grams from the SuccessorArray
ngramOf :: SuccessorArray a -> Int -> [[a]]
ngramOf s n = filter ((== n) . length) $ map (take n) $ toList s

ngramOfList :: Ord a => [a] -> Int -> [[a]]
ngramOfList c n = ngramOf (fromList c) n

prop_length_ngramOf :: Int -> SuccessorArray Int -> Property
prop_length_ngramOf n (SuccessorArray c i) =
    V.length c >= n && n > 0 ==>
    (1+ V.length c -n) == (length $ ngramOf (SuccessorArray c i) n)

-- Like toList, but returns a Vector of Vectors instead of list of lists
elems :: SuccessorArray a -> V.Vector (V.Vector a)
elems (SuccessorArray c i) = V.map vectorAt i
  where vectorAt index = V.drop index c

ngramFromElems :: SuccessorArray a -> Int -> V.Vector (V.Vector a)
ngramFromElems sa n = V.filter ((== n) . V.length) $ V.map (V.take n) $ elems sa

binarySearch :: Integral a => (a -> Ordering) -> (a, a) -> Maybe a
binarySearch p (low, high)
  | high < low = Nothing
  | otherwise =
      let mid = (low + high) `div` 2 in
      case p mid of
        LT -> binarySearch p (low, mid-1)
        GT -> binarySearch p (mid+1, high)
        EQ -> Just mid

binarySearchVector :: Ord a => V.Vector a -> a -> Maybe Int
binarySearchVector v item = binarySearch pivot (0, V.length v - 1)
  where pivot index = item `compare` (v V.! index)

-- Checks whether or not a (sub)vector is contained in the SuccessorArray
contains :: Ord a => SuccessorArray a -> V.Vector a -> Bool
contains s subVector = case binarySearchVector (shorten s) subVector of
                         Just _ -> True
                         Nothing -> False
  where shorten = V.map (V.take $ V.length subVector) . elems

-- Finds the lowest index of a given item in an ordered array by binarysearch
lowerIndexOf :: Ord a => V.Vector a -> a -> Int -> Maybe Int
lowerIndexOf vec item high = lowerIndexOfBy pivot (0, high)
  where pivot index = item `compare` (vec V.! index)

lowerIndexOfBy :: Integral a => (a -> Ordering) -> (a, a) -> Maybe a
lowerIndexOfBy p (low, high)
  | low == high = Just low
  | otherwise = let mid = (low + high) `div` 2 in
                case p mid of
                  GT -> lowerIndexOfBy p (mid+1, high)
                  _  -> lowerIndexOfBy p (low, mid)

-- Finds the highest index of a given item in an ordered array by binarysearch
upperIndexOf :: Ord a => V.Vector a -> a -> Int -> Maybe Int
upperIndexOf vec item low = upperIndexOfBy pivot (low, V.length vec - 1)
  where pivot index = item `compare` (vec V.! index)

upperIndexOfBy :: Integral a => (a -> Ordering) -> (a, a) -> Maybe a
upperIndexOfBy p (low, high)
  | high == low = Just low
  | otherwise = case p (mid+1) of
                  LT -> upperIndexOfBy p (low, mid)
                  _  -> upperIndexOfBy p (mid+1, high)
  where mid = ((low + high) `div` 2)

-- Finds the number of times a given item occurs in an ordered array
frequencyOf :: Ord a => V.Vector a -> a -> Maybe Int
frequencyOf vec item
  | isNothing pivot = Nothing
  | otherwise = do
      let index = fromJust pivot
      lower <- lowerIndexOf vec item index
      upper <- upperIndexOf vec item index
      return (upper-lower+1)
  where pivot = binarySearchVector vec item

prop_frequencyOf :: [Int] -> Int -> Bool
prop_frequencyOf list item = frequencyOf vec item == listFreq list item
  where vec = V.fromList $ L.sort list
        listFreq :: [Int] -> Int -> Maybe Int
        listFreq list item | freq > 0 = Just freq
                           | otherwise = Nothing
        freq = length $ L.elemIndices item list

-- Finds the number of times a given sequence occurs within the suffixarray
containsWithFrequency :: Ord a => SuccessorArray a -> V.Vector a -> Maybe Int
containsWithFrequency sa vec 
  | contains sa vec = frequencyOf (ngramFromElems sa (V.length vec)) vec
  | otherwise = Nothing

-- Finds the most common ngram(s) for a given n in a suffixarray
mostFrequentNgram :: Ord a => SuccessorArray a -> Int -> Maybe ([V.Vector a], Int)
mostFrequentNgram sa n 
  | V.length ngrams == 0 = Nothing
  | otherwise = Just $ maxNgram $ ngramFrequencies ngrams
    where ngrams = ngramFromElems sa n

maxNgram :: Ord a => [(V.Vector a, Int)] -> ([V.Vector a], Int)
maxNgram ngs = (winners, max)
  where max      = maximum $ snd $ unzip ngs
        winners  = fst $ unzip $ filter (\n -> snd n == max) ngs

ngramFrequencies :: Ord a => V.Vector (V.Vector a) -> [(V.Vector a, Int)]
ngramFrequencies ngs = L.nub $ V.toList $ V.zip ngs lens
  where lens = V.map (\n -> fromJust $ frequencyOf ngs n) ngs

prop_ngramFrequencies :: SuccessorArray Int -> Int -> Property
prop_ngramFrequencies (SuccessorArray c i) n =
    V.length c >= n ==>
    (all (\ngram -> fromJust (frequencyOf body (fst ngram)) == snd ngram) (ngramFrequencies body))
  where body  = ngramFromElems (SuccessorArray c i) n
