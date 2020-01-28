
main :: IO()
main = interact (writeOutput . solve . readInput)

readInput :: String -> [String]
readInput = filter (/= "END") .  lines  

solve :: [String] -> [Int]
solve = map (\s -> length (solve' s))

solve' :: String -> [String]
solve' "1" = ["1"]
solve' s = [s] ++ solve' (show (length s))

writeOutput :: [Int] -> String
writeOutput = unlines . map show
