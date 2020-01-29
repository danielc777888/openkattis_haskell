import Text.Printf

type AtBat = Int


main :: IO()
main = interact (writeOutput . solve . readInput)

readInput :: String -> (Int, [AtBat])
readInput s = (n, atBats)
  where l = lines s
        n = read (l !! 0)
        atBats = map read (words (l !! 1))


solve :: (Int, [AtBat]) -> Double
solve (n, as) =  (fromIntegral hits)  / (fromIntegral (n - walks))
  where counts = foldl ( \(x1, y1) x -> (if x == -1 then x1 + 1 else x1, if x /= -1 then y1 + x else y1 )  ) (0,0) as
        walks = fst counts
        hits = snd counts
        

writeOutput :: Double -> String
writeOutput = printf "%.8f"

