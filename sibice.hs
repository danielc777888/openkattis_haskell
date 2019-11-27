
readInput :: String -> [String]
readInput = lines

solve :: [String] -> [String]
solve (x:xs) = map (\x -> if (read x) > hyp then "NE" else "DA") xs
  where header = map read (words x)
        m = header !! 0
        w = header !! 1
        h = header !! 2
        hyp = sqrt (w*w + h*h)
        


writeOutput :: [String] -> String
writeOutput  = unlines


main :: IO()
main = interact (writeOutput . solve . readInput)
