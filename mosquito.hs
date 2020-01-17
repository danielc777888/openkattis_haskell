
data LifeCycle = LifeCycle { mosquitoes :: Int
                           , pupae :: Int
                           , larvae :: Int
                           , eggs :: Int
                           , larvaeSurvivalRate :: Int
                           , pupaeSurvivalRate :: Int
                           , weeks :: Int }

                 
main :: IO()
main = interact (writeOutput . solve . readInput)

readInput :: String -> [LifeCycle]
readInput s = map (\x -> parseLifeCycle x) (lines s)

parseLifeCycle :: String -> LifeCycle
parseLifeCycle s = LifeCycle { mosquitoes = read (ws !! 0) 
                         , pupae = read (ws !! 1)
                         , larvae = read (ws !! 2)
                         , eggs = read (ws !! 3)
                         , larvaeSurvivalRate = read (ws !! 4)
                         , pupaeSurvivalRate = read (ws !! 5)
                         , weeks = read (ws !! 6)}
                where ws =  words s               
               

solve :: [LifeCycle] -> [Int]
solve l = map (\x -> sum [eggs x, larvae x] ) l

writeOutput :: [Int] -> String
writeOutput xs =  unlines $ map show xs
  

 
