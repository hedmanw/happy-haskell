import Test.QuickCheck
import Data.Maybe
import qualified Data.Vector as V
import qualified Data.List as L

text = words "to be or not to be"
corpus = fromList text

-- SuffixArray = SuccessorList av en corpus = base text och index för orden
data SuffixArray a = SuffixArray (V.Vector a) (V.Vector Int)
  deriving Show

instance (Arbitrary a, Ord a) => Arbitrary (SuffixArray a) where
  arbitrary = do
    list <- arbitrary
    return (fromList list)

-- Compare subvectors at indices i and j of corpus c.
saCompare :: Ord a => V.Vector a -> Int -> Int -> Ordering
saCompare c i j = compare (V.drop i c) (V.drop j c)

-- Construct a SuffixArray from a single vector containing the corpus
suffixArray :: Ord a => V.Vector a -> SuffixArray a
suffixArray c = SuffixArray c (V.fromList wordIndices)
  where corpusSize = V.length c -1
        allIndices = [0..corpusSize]
        wordIndices = L.sortBy (saCompare c) allIndices

-- Make a SuffixArray from a List
fromList :: Ord a => [a] -> SuffixArray a
fromList = suffixArray . V.fromList

-- Make a list of lists containing each suffix sequence, in order of the
-- first item of the suffix sequence.
toList :: SuffixArray a -> [[a]]
toList (SuffixArray c i) = V.foldr vectorAt [] i
  where vectorAt index l = V.toList (V.drop index c) : l

-- Get all n-grams from the SuffixArray
ngramOf :: Int -> SuffixArray a -> [[a]]
ngramOf n s = filter ((== n) . length) $ map (take n) $ toList s

ngramOfList :: Ord a => Int -> [a] -> [[a]]
ngramOfList n c = ngramOf n (fromList c)

prop_length_ngramOf :: Int -> SuffixArray Int -> Property
prop_length_ngramOf n (SuffixArray c i) =
    V.length c >= n && n > 0 ==>
    (1+ V.length c -n) == (length $ ngramOf n (SuffixArray c i))

-- Like toList, but returns a Vector of Vectors instead of list of lists
elems :: SuffixArray a -> V.Vector (V.Vector a)
elems (SuffixArray c i) = V.map vectorAt i
  where vectorAt index = V.drop index c

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

-- Checks whether or not a (sub)vector is contained in the SuffixArray
contains :: Ord a => SuffixArray a -> V.Vector a -> Bool
contains s subVector = case binarySearchVector (shorten s) subVector of
                         Just _ -> True
                         Nothing -> False
  where shorten = V.map (V.take $ V.length subVector) . elems

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


