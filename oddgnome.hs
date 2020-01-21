import Data.List

main :: IO()
main = interact (writeOutput . solve . readInput)

readInput :: String -> [[Int]]
readInput xs = map(\l -> map read (tail (words l)))  (tail (lines xs))

solve :: [[Int]] -> [Int]
solve = map (findGnome 1)

findGnome :: Int -> [Int] -> Int
findGnome n [] = n + 1
findGnome n (x:y:xs) = if y > x && (y - x) == 1 then findGnome (n+1) (y:xs)
  else n + 1
  

writeOutput :: [Int] -> String
writeOutput xs = unlines $ map show xs

 

