
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
solve = map(\l -> mosquitoes (simulate l (weeks l)) )

simulate :: LifeCycle -> Int -> LifeCycle
simulate l 0 = l
simulate l n = simulate (newLifeCycle l) (n-1)

newLifeCycle :: LifeCycle -> LifeCycle
newLifeCycle l = LifeCycle { mosquitoes = simMosquitoes (pupae l) (pupaeSurvivalRate l) 
                         , pupae = simPupa (larvae l) (larvaeSurvivalRate l)
                         , larvae = simLarvae (mosquitoes l) (eggs l)
                         , eggs = eggs l
                         , larvaeSurvivalRate = larvaeSurvivalRate l
                         , pupaeSurvivalRate = pupaeSurvivalRate l
                         , weeks = weeks l}

simLarvae :: Int -> Int -> Int
simLarvae mosquitos eggs = mosquitos * eggs

simPupa :: Int -> Int -> Int
simPupa larvae survivalRate = larvae `div` survivalRate

simMosquitoes :: Int -> Int -> Int
simMosquitoes pupa survivalRate = pupa `div` survivalRate


writeOutput :: [Int] -> String
writeOutput xs =  unlines $ map show xs
  

 
