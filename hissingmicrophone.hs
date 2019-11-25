solve :: String -> String
solve [] = []
solve xs =  if any (\(x, y) -> x == 's' && y == 's') (zip xs (tail xs)) then "hiss" else "no hiss" 


readInput :: String -> String
readInput s = s

writeOutput :: String -> String  
writeOutput s = s

main :: IO()
main = interact (writeOutput .solve . readInput)
