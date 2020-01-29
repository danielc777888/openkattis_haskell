{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TI

type Strength = Int
type MonsterArmy = [Strength]
type Battle = (MonsterArmy, MonsterArmy)

main :: IO()
main = TI.interact (writeOutput . solve . readInput)

readInput :: T.Text -> [Battle]
readInput s = createBattles (filter (/= "") ( tail (T.lines s )))

createBattles :: [T.Text] -> [Battle]
createBattles [] = []
createBattles xs = [(army1, army2)] ++ createBattles (drop 3 xs)
  where armies = tail (take 3 xs)
        army1 = map (\t -> read (T.unpack t))  (T.words (armies !! 0))
        army2 = map (\t -> read (T.unpack t))  (T.words (armies !! 1))

solve :: [Battle] -> [T.Text]
solve = map solve'

solve' :: (MonsterArmy, MonsterArmy) -> T.Text
solve' (m1, m2) = if maxM1 == maxM2 then "Godzilla"
                  else if maxM2 > maxM1 then "MechaGodzilla"
                  else "Godzilla"
  where maxM2 = maximum m2
        maxM1 = maximum m1

writeOutput :: [T.Text] -> T.Text
writeOutput = T.unlines

