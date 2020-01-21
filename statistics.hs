
main :: IO()
main = interact (writeOutput . solve . readInput)

readInput :: String -> [[Int]]
readInput xs = map (\l -> map read (words (tail l))) (lines xs)

solve :: [[Int]] -> [String]
solve xs = map stats (zip xs [1..])

stats :: ([Int], Int) -> String
stats (xs,i) = "Case " ++ show i ++ ": " ++ show mn ++ " " ++ show mx ++ " " ++ show range
  where mn = minimum xs
        mx = maximum xs
        range = mx - mn

writeOutput :: [String] -> String
writeOutput = unlines

 
