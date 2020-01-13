
main :: IO()
main = interact (writeOutput . solve . readInput)

readInput :: String -> [(Int, Int)]
readInput = map (\l -> readLine (words l)) . lines

readLine :: [String] -> (Int, Int)
readLine (x:y:[]) = (read x, read y)

solve :: [(Int, Int)] -> [String]
solve ((0,0):xs) = []
solve ((x,y):xs)
  | x + y == 13 = ["Never speak again."] ++ solve xs
  | x > y = ["To the convention."] ++ solve xs
  | x < y = ["Left beehind."] ++ solve xs
  | otherwise = ["Undecided."] ++ solve xs
                  
writeOutput :: [String] -> String
writeOutput = unlines
