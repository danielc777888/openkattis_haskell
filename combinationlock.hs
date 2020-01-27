
main :: IO()
main = interact (writeOutput . solve . readInput)

readInput :: String -> [[Int]]
readInput = map (map read . words) . filter (/= "0 0 0 0") .  lines  

solve :: [[Int]] -> [Int]
solve = map solve'

solve' :: [Int] -> Int
solve' (s:n1:n2:n3:[]) = (step1 + step2 + step3) * 9
  where step1 = 80 + (wrapAround s n1 - n1 + s)
        step2 = 40 + (wrapAround n2 n1 - n1 + n2)
        step3 = n2 + (wrapAround n2 n3 - n3)
        wrapAround x y = if x < y then 40 else 0

writeOutput :: [Int] -> String
writeOutput = unlines . map show

