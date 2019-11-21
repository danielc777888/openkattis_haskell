solve :: [Int] -> Int
solve xs = solve' a b
  where a = xs !! 0
        b = xs !! 1

solve' :: Int -> Int -> Int
solve' a b
  | a > 0 && b > 0 = 1
  | a < 0 && b > 0 = 2
  | a < 0 && b < 0 = 3
  | otherwise = 4

readInput :: String -> [Int]
readInput = (map read) . words

writeOutput :: Int -> String  
writeOutput = show

main :: IO()
main = interact (writeOutput . solve . readInput)
