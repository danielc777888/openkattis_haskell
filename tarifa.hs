readInput :: String -> [Int]
readInput xs = map (read :: String -> Int) (lines xs)

solve :: [Int] -> Int
solve (a:b:xs) = a * b - sum xs + a

writeOutput :: Int -> String
writeOutput = show

main :: IO()
main = interact (writeOutput . solve . readInput)
