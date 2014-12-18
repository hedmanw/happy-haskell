import qualified Data.Vector as V
import qualified Data.List as L

-- SuffixArray = SuccessorList av en corpus = base text och index för orden
data SuffixArray a = SuffixArray (V.Vector a) (V.Vector Int)
  deriving Show

-- Compare subvectors at indices i and j of corpus c using a given compare function.
saCompare :: Ord a => (V.Vector a -> V.Vector a -> Ordering) ->
             V.Vector a -> Int -> Int -> Ordering
saCompare cmp c i j = cmp (V.drop i c) (V.drop j c)

suffixArray :: Ord a => V.Vector a -> SuffixArray a
suffixArray c = SuffixArray c (V.fromList wordIndices)
  where corpusSize = V.length c -1
        allIndices = [0..corpusSize]
        wordIndices = L.sortBy (saCompare compare c) allIndices

fromList :: Ord a => [a] -> SuffixArray a
fromList = suffixArray . V.fromList

toList :: SuffixArray a -> [[a]]
toList (SuffixArray d i) = V.foldr vecAt [] i
  where vecAt idx l = V.toList (V.drop idx d) : l
