import Data.Char

type CardRank = Int
type Hand = [CardRank]

main :: IO()
main = interact (writeOutput . solve . readInput)

readInput :: String -> Hand
readInput xs = map (\w -> rank (w !! 0)) (words xs)

rank :: Char -> Int
rank c
  | c == 'A' = 1
  | c == 'T' = 10
  | c == 'J' = 11
  | c == 'Q' = 12
  | c == 'K' = 13
  | otherwise = digitToInt c


solve :: Hand -> Int
solve h =  maximum counts
  where counts = foldl (\xs y -> (take (y-1) xs)  ++ [(xs !! (y - 1)) + 1] ++ (drop y xs)) [0,0,0,0,0,0,0,0,0,0,0,0,0] h

writeOutput :: Int -> String
writeOutput = show

