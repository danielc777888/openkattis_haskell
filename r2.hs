solve [] = []
solve (a:b:rest) = (2 * b) - a :(solve rest)

readInput = (map read) . words
writeOutput = unlines . (map show)

main = interact (writeOutput . solve . readInput)

