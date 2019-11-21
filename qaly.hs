solve :: [Double] -> Double
solve [] = 0.0
solve (n:ps) = sum (solve' ps)

solve' :: [Double] -> [Double]
solve' [] = []
solve' (a:b:ps) = a * b : solve' ps

readInput :: String -> [Double]
readInput = (map read) . words

writeOutput :: Double -> String  
writeOutput = show

main :: IO()
main = interact (writeOutput . solve . readInput)
