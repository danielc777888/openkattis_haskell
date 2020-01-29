
type Strength = Int
type MonsterArmy = [Strength]
type Battle = (MonsterArmy, MonsterArmy)

main :: IO()
main = interact (writeOutput . solve . readInput)

readInput :: String -> [Battle]
readInput s = createBattles (filter (/= "") ( tail (lines s )))
  --[([1], [1]), ([1,3,2], [5,5])] --map (roll . map read . words) . filter (/= "") .  lines  . tails

createBattles :: [String] -> [Battle]
createBattles [] = []
createBattles xs = [(army1, army2)] ++ createBattles (drop 3 xs)
  where armies = tail (take 3 xs)
        army1 = map read (words (armies !! 0))
        army2 = map read (words (armies !! 1))

solve :: [Battle] -> [String]
solve = map solve'

solve' :: (MonsterArmy, MonsterArmy) -> String
solve' (m1, m2) = if maxM1 == maxM2 then "Godzilla"
                  else if maxM2 > maxM1 then "MechaGodzilla"
                  else "Godzilla"
  where maxM2 = maximum m2
        maxM1 = maximum m1

writeOutput :: [String] -> String
writeOutput = unlines

