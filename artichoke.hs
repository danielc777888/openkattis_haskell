import Text.Printf

main :: IO()
main = interact (writeOutput . solve . readInput)

readInput :: String -> [Double]
readInput = (map read) . words

solve :: [Double] -> Double
solve xs = if maxDecline <= 0.0 then 0.0 else maxDecline
  where p = xs !! 0
        a = xs !! 1
        b = xs !! 2
        c = xs !! 3
        d = xs !! 4
        n = xs !! 5
        maxDecline = snd $ foldl ( \(x1, y1) x -> let  pr = price x p a b c d
                                                       diff = x1 - pr in (if pr > x1 then pr  else x1, if diff > y1 then diff else y1 )) (price1,0.0) [2.0..n]
        price1 = price 1 p a b c d
        

price :: Double -> Double -> Double -> Double -> Double -> Double -> Double
price k p a b c d = p * (sin(a * k + b) + cos(c * k + d) + 2.0)

writeOutput :: Double -> String
writeOutput = printf "%.8f"

