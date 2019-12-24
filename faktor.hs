main :: IO()
main = interact (writeOutput . solve . readInput)

readInput :: String -> (Int, Int)
readInput s = (read (w !! 0), read (w !! 1))
              where w = words s

solve :: (Int, Int) -> Int
solve (a, i) = (i - 1) * a + 1

writeOutput :: Int -> String
writeOutput = show
