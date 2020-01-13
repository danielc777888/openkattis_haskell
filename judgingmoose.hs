

main :: IO()
main = interact (solve . readInput)

readInput :: String -> [Int]
readInput s = map read (words s)

solve :: [Int] -> String
solve xs = if lTines == 0 && rTines == 0 then "Not a moose"
           else if lTines == rTines then "Even " ++ show (lTines * 2)
           else "Odd " ++ show (2 * max lTines rTines)
  where lTines = xs !! 0
        rTines = xs !! 1
