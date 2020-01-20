import Data.List

main :: IO()
main = interact (solve . readInput)

readInput :: String -> [Int]
readInput s = map read (words (l !! 1))
  where l = lines s

solve :: [Int] -> String
solve xs = show (snd (minimumBy compare t))
  where t = zip xs [0..]
