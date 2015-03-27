import Data.Char

printDiamond :: Char -> IO()
printDiamond c = do
  let diamond = getDiamond c
  putStrLn $ unlines diamond

getDiamond :: Char -> [String]
getDiamond 'A' = ["A"]
getDiamond c = (padLeft 'A' c : wholeDiamond) ++ [padLeft 'A' c]
  where wholeDiamond = map (\line -> line ++ padRight (head $ reverse line) c) halfDiamond
        halfDiamond = map (\line -> padLeft line c) (completeList c)

completeList :: Char -> [Char]
completeList c = letterList c ++ (tail $ reverse $ letterList c)

letterList :: Char -> [Char]
letterList c = ['B'..c]

padLeft :: Char -> Char -> String
padLeft current max = pad spaces current
  where spaces = letterDiff current max

padRight :: Char -> Char -> String
padRight current max = pad spaces current
  where spaces = ((letterDiff 'A' current) - 1) * 2 + 1

pad :: Int -> Char -> String
pad spaces current = reverse $ current : replicate spaces ' '

letterDiff :: Char -> Char -> Int
letterDiff a b = ord b - ord a

