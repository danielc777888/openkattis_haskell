import Data.List

main :: IO()
main = interact (writeOutput . solve . readInput)

readInput :: String -> [[Int]]
readInput xs = map(\l -> map read (tail (words l)))  (tail (lines xs))

solve :: [[Int]] -> [Int]
solve = map findGnome

findGnome :: [Int] -> Int
findGnome xs = case elemIndex (head (dropWhile (\(a,b) -> a == b) z)) z  of
  Just n -> n + 1
  Nothing -> error "gnome not found!!"
  where z = (zip xs [(head xs)..])

writeOutput :: [Int] -> String
writeOutput xs = unlines $ map show xs

 

