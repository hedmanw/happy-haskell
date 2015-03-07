printMagicMatrix :: Int -> IO ()
printMagicMatrix n = do
    let matrix = magicMatrix n
    let rows = map (\row -> show row) matrix
    putStrLn $ unlines rows

magicMatrix :: Int -> [[Int]]
magicMatrix n = [matrixRow n x | x <- [1..n]]

matrixRow :: Int -> Int -> [Int]
matrixRow n i = [1..i] ++ replicate (n-i) i

