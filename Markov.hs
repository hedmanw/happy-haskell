import System.Random
import Data.Char
import Data.Ord
import Data.List
import Test.QuickCheck

-- Markov generator
-- in haskell!
-- Jonathan Thunberg
-- Wilhelm Hedman

text :: String
--text = "of the people, by the people, for the people"
text = "Slow lorises have a round head, narrow snout, large eyes, and a variety of distinctive coloration patterns that are species-dependent. Their arms and legs are nearly equal in length, and their trunk is long, allowing them to twist and extend to nearby branches. The hands and feet of slow lorises have several adaptations that give them a pincer-like grip and enable them to grasp branches for long periods of time. Slow lorises have a toxic bite, a trait rare among mammals and unique to lorisid primates. The toxin is obtained by licking a gland on their arm, and the secretion is activated by mixing with saliva. Their toxic bite is a deterrent to predators, and the toxin is also applied to the fur during grooming as a form of protection for their infants. They move slowly and deliberately, making little or no noise, and when threatened, they stop moving and remain motionless. Their only documented predators—apart from humans—include snakes, hawk-eagles and orangutans, although cats, civets and sun bears are suspected. Little is known about their social structure, but they are known to communicate by scent marking. Males are highly territorial. Slow lorises reproduce slowly, and the infants are initially parked on branches or carried by either parent. They are omnivores, eating small animals, fruit, tree gum, and other vegetation."

-- Dict is a list of string tuples, where the second string is a word that follows after the first string
type Dict = [(String, String)]

-- Delimiters for sentences. Could probably be different depending on input language.
sentenceSeparator = [".", "!", "?"]

-- Is a certain string at the end of a sentence?
endOfSentence :: String -> Bool
endOfSentence s = any (\sep -> sep `isSuffixOf` s) sentenceSeparator

wordList :: String -> [String]
wordList = words

-- Makes a Markov Dictionary from a list of words.
-- The Markov Dictionary contains each word in the list,
-- and the word which comes directly after it. The last
-- word in the list is not contained in the dictionary,
-- because it has no successor.
wordOrder2 :: [String] -> Dict
wordOrder2 []     = []
wordOrder2 (x:[]) = [(x, "")]
wordOrder2 (f:x:xs) 
           | endOfSentence f = (f,"") : wordOrder2 (x:xs)
           | otherwise = (f,x) : wordOrder2 (x:xs)

prop_length_wordOrder2 :: [String] -> Property
prop_length_wordOrder2 words = length words > 0 ==>
  (length words -1) == (length $ wordOrder2 words)

prop_successor_wordOrder2 :: [String] -> Bool
prop_successor_wordOrder2 words = checkSuccessor words (wordOrder2 words)
  where checkSuccessor :: [String] -> Dict -> Bool
        checkSuccessor [] _  = True 
        checkSuccessor (w:[]) [] = True
        checkSuccessor (w:nw:ws) (d:ds)
          | endOfSentence w
            = snd d == "" && checkSuccessor (nw:ws) ds
          | otherwise
            = w == fst d && nw == snd d && checkSuccessor (nw:ws) ds

-- Helper for debugging
sortByWord :: Dict -> Dict
sortByWord = sortBy (comparing fst)

-- Helper for debugging
sortedList :: String -> Dict
sortedList s = sortByWord $ wordOrder2 $ wordList s

-- Randomly picks a sucessor to follow the given word from the dictionary.
getSuccessor :: Dict -> String -> StdGen -> (String, StdGen)
getSuccessor dict word gen = chooseWord (filter (\w -> fst w == word) dict) gen

-- Chooses the successor to a given word
chooseWord :: Dict -> StdGen -> (String, StdGen)
chooseWord []   gen = ("", gen)
chooseWord dict gen = (snd (dict !! index),nextGen)
  where (index, nextGen) = randomR (0, length dict-1) gen

data MarkovDict = Markov Dict
  deriving Show

instance Arbitrary MarkovDict where
  arbitrary = generateMarkovDict

instance Arbitrary StdGen where
  arbitrary = do n <- arbitrary
                 return (mkStdGen n)

generateMarkovDict :: Gen MarkovDict
generateMarkovDict = do
  words <- listOf1 $ listOf1 $ elements (['.', '!', '?'] ++ ['a'..'z'])
  return (Markov (wordOrder2 words))

prop_word_getSuccessor :: MarkovDict -> StdGen -> Bool
prop_word_getSuccessor (Markov dict) gen = fst successor `elem` legalWords
  where (index, nextGen) = randomR (0, length dict-1) gen
        successor = getSuccessor dict word nextGen
        word = fst (dict !! index)
        tuples = filter (\t -> fst t == word) dict
        legalWords = map snd tuples

-- Chooses a whole sentence, word by word, given a starting word for the sentence.
getSentence :: Dict -> String -> StdGen -> String
getSentence dict start gen = unwords $ reverse $ buildSentence dict [start] gen
  where buildSentence :: Dict -> [String] -> StdGen -> [String]
        buildSentence dict sen gen
         | fst (getSuccessor dict (head sen) gen) == "" = sen
         | otherwise = 
           let succ = getSuccessor dict (head sen) gen in
           buildSentence dict (fst succ : sen) (snd succ)

doEverything :: Dict -> String -> StdGen -> String
doEverything dict start gen = getSentence dict start gen

printMarkovText :: FilePath -> IO ()
printMarkovText f = do
               gen <- newStdGen
               text <- readText f
               let list = wordOrder2 $ wordList text
               let upperList = filter (\t -> isAsciiUpper $ head $ fst t) list
               let (seed, nextGen) = randomR (0, length upperList-1) gen
               putStrLn $ getSentence list (fst $ upperList !! seed) nextGen

readText :: FilePath -> IO String
readText = readFile
