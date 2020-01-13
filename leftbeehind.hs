
main :: IO()
main = interact (writeOutput . solve . readInput)

readInput :: String -> [(Int, Int)]
readInput s = map (\l -> (read (words l !! 0) :: Int, read (words l !! 1) :: Int )) (lines s)

solve :: [(Int, Int)] -> [String]
solve xs = filter (/="") $ map (\(a,b) -> solve' a b) xs

solve' :: Int -> Int -> String
solve' x y
  | x == 0 && y == 0 = ""
  | x + y == 13 = "Never speak again."
  | x > y = "To the convention."
  | x < y = "Left beehind."
  | otherwise = "Undecided."
                  
writeOutput :: [String] -> String
writeOutput s = unlines s
