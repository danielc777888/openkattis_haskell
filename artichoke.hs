import Text.Printf

main :: IO()
main = interact (writeOutput . solve . readInput)

readInput :: String -> [Float]
readInput = (map read) . words

solve :: [Float] -> Float
solve xs = if maxDecline <= 0.0 then 0.0 else maxDecline
  where p = xs !! 0
        a = xs !! 1
        b = xs !! 2
        c = xs !! 3
        d = xs !! 4
        n = xs !! 5
        prices = price n p a b c d
        maxDecline = maximum (maxDiffs prices)

price :: Float -> Float -> Float -> Float -> Float -> Float -> [Float]
price 0 _ _ _ _ _ = []
price k p a b c d = price (k-1) p a b c d ++ [p * (sin(a * k + b) + cos(c * k + d) + 2)]

maxDiffs :: [Float] -> [Float]
maxDiffs [] = []
maxDiffs [x] = [0.0]
maxDiffs (x:xs) = [maximum (map (\y -> x - y) xs)] ++ maxDiffs xs

writeOutput :: Float -> String
writeOutput = printf "%.8f"

