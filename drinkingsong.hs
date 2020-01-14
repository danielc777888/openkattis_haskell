data Song = Song NumberOfBottles Beverage deriving (Show)

type NumberOfBottles =  Int
type Beverage = String

main :: IO()
main = interact (writeOutput . solve . readInput)

solve :: Song -> [String]
solve (Song n b) = solve' n b

solve' :: Int -> String -> [String]
solve' 1 b = ["1 bottle of " ++ b ++ " on the wall, 1 bottle of " ++ b ++ "."] ++ ["Take it down, pass it around, no more bottles of " ++ b ++ "."]
solve' n b = [show n ++ " bottles of " ++ b ++ " on the wall, " ++ show n ++ " bottles of " ++ b ++ "."]
  ++ ["Take one down, pass it around, " ++ show (n-1) ++ " " ++ bottles ++ " of " ++ b ++ " on the wall.\n"] ++ solve' (n-1) b
  where bottles = pluralize (n - 1)
  
pluralize :: Int -> String
pluralize n
  | n == 1 = "bottle"
  | otherwise = "bottles"
  

readInput :: String -> Song
readInput s = Song (read (l !! 0)) (l !! 1)
  where l = lines s

writeOutput :: [String] -> String
writeOutput = unlines
