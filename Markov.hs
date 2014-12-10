import System.Random
import Data.Char
import Data.Ord
import Data.List
-- Markov generator
-- in haskell!
-- Jonathan Thunberg
-- Wilhelm Hedman
--

text :: String
--text = "of the people, by the people, for the people"
text = "Slow lorises have a round head, narrow snout, large eyes, and a variety of distinctive coloration patterns that are species-dependent. Their arms and legs are nearly equal in length, and their trunk is long, allowing them to twist and extend to nearby branches. The hands and feet of slow lorises have several adaptations that give them a pincer-like grip and enable them to grasp branches for long periods of time. Slow lorises have a toxic bite, a trait rare among mammals and unique to lorisid primates. The toxin is obtained by licking a gland on their arm, and the secretion is activated by mixing with saliva. Their toxic bite is a deterrent to predators, and the toxin is also applied to the fur during grooming as a form of protection for their infants. They move slowly and deliberately, making little or no noise, and when threatened, they stop moving and remain motionless. Their only documented predators—apart from humans—include snakes, hawk-eagles and orangutans, although cats, civets and sun bears are suspected. Little is known about their social structure, but they are known to communicate by scent marking. Males are highly territorial. Slow lorises reproduce slowly, and the infants are initially parked on branches or carried by either parent. They are omnivores, eating small animals, fruit, tree gum, and other vegetation."

wordList :: String -> [String]
wordList = words

wordOrder2 :: [String] -> [(String,String)]
wordOrder2 (x:[]) = []
wordOrder2 (f:x:xs) 
           | "." `isSuffixOf` f = (f,"") : wordOrder2 (x:xs)
           | otherwise = (f,x) : wordOrder2 (x:xs)

sortByWord :: [(String,String)] -> [(String,String)]
sortByWord = sortBy (comparing fst)

sortedList :: String -> [(String,String)]
sortedList s = sortByWord $ wordOrder2 $ wordList s

getSuccessor :: [(String,String)] -> String -> StdGen -> (String, StdGen)
getSuccessor dict word stdgen = choose (filter (\w -> fst w == word) dict) stdgen

choose :: [(String, String)] -> StdGen -> (String, StdGen)
choose []   gen = ("", gen)
choose dict gen = (snd (dict !! index),nextGen)
  where (index, nextGen) = randomR (0, length dict-1) gen

getSentence :: [(String,String)] -> String -> StdGen -> String
getSentence dict start gen = unwords $ reverse $ buildSentence dict [start] gen
  where buildSentence :: [(String,String)] -> [String] -> StdGen -> [String]
        buildSentence dict sen gen
         | fst (getSuccessor dict (head sen) gen) == "" = sen
         | otherwise = 
           let succ = getSuccessor dict (head sen) gen in
           buildSentence dict (fst succ : sen) (snd succ)

printMarkovText :: FilePath -> IO ()
printMarkovText f = do
               gen <- newStdGen
               text <- readText f
               let list = sortedList text
               let upperList = filter (\t -> isAsciiUpper $ head $ fst t) list
               let (seed, nextGen) = randomR (0, length upperList-1) gen
               putStrLn $ getSentence list (fst $ upperList !! seed) nextGen

readText :: FilePath -> IO String
readText = readFile
