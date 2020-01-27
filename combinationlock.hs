
main :: IO()
main = interact (writeOutput . solve . readInput)

readInput :: String -> [[Int]]
readInput = map (map read . words) .  lines  

solve :: [[Int]] -> [Int]
solve = map sum

writeOutput :: [Int] -> String
writeOutput = unlines . map show

