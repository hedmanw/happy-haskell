import Test.QuickCheck
import qualified Data.Vector as V
import qualified Data.List as L

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

ngramOf :: Int -> SuffixArray a -> [[a]]
ngramOf n s = filter ((== n) . length) $ map (take n) $ toList s

prop_length_ngramOf :: Int -> SuffixArray String -> Property
prop_length_ngramOf n (SuffixArray c i) =
    V.length c >= n ==> (1+ V.length c -n) == (length $ ngramOf n (SuffixArray c i))

