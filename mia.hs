
type Die = Int
data Roll = Roll Die Die

main :: IO()
main = interact (writeOutput . solve . readInput)

readInput :: String -> [(Roll, Roll)]
readInput = map (roll . map read . words) . filter (/= "0 0 0 0") .  lines  

roll :: [Int] -> (Roll, Roll)
roll xs  = (Roll (xs !! 0) (xs !! 1), Roll (xs !! 2) (xs !! 3))

solve :: [(Roll, Roll)] -> [String]
solve = map solve'

solve' :: (Roll, Roll) -> String
solve' (r1, r2) = if mia r1 && not (mia r2) then player1
              else if mia r2 && not (mia r1) then player2
              else if double r1 && not (double r2) then player1
              else if double r2 && not (double r1) then player2
              else if sumRoll r1 > sumRoll r2 then player1
              else if sumRoll r2 > sumRoll r1 then player2
              else "Tie."

player1 = win 1
player2 = win 2

win :: Int -> String
win x = "Player " ++ show x ++ " wins."

mia :: Roll -> Bool
mia (Roll d1 d2)  = (d1 == 1 && d2 == 2)  || (d1 == 2 && d2 == 1)

double :: Roll -> Bool
double (Roll d1 d2) = d1 == d2

sumRoll :: Roll -> Int
sumRoll (Roll d1 d2) = read (show maxRoll ++ show minRoll)
  where maxRoll = maximum [d1, d2]
        minRoll = minimum [d1, d2]

writeOutput :: [String] -> String
writeOutput = unlines

