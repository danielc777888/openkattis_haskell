
readInput :: String -> Int
readInput = read

solve :: Int -> [String]
solve n = map (\(x, y) -> x ++ " " ++ y) (zip (map show [1..n]) (repeat "Abracadabra"))

writeOutput :: [String] -> String
writeOutput  = unlines


main :: IO()
main = interact (writeOutput . solve . readInput)
