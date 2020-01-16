

--module Acm where

type Minutes = Int
type Problem = Char

data Result = Correct | Wrong deriving (Eq)

instance Show Result where
   show Correct = "right"
   show Wrong = "wrong"

data Submission = Submission Minutes Problem Result

main :: IO()
main = interact (writeOutput . solve . readInput)

readInput :: String -> [Submission]
readInput s = [parseSubmission line | line <- lines s, line /= "-1"]

parseSubmission :: String -> Submission
parseSubmission s = Submission minutes problem result
  where w = words s
        minutes = read (w !! 0)
        problem = (w !! 1) !! 0
        result = if (w !! 2 == "right") then Correct else Wrong

solve :: [Submission] -> (Int, Int)
solve s = (problemsSolved s, totalTime s)

problemsSolved :: [Submission] -> Int
problemsSolved ss = length (filter (\s -> (result s) == Correct)  ss)

totalTime :: [Submission] -> Int
totalTime ss = sum (map (\s -> (minutes s) + (penalties ss s))  (filter (\s -> (result s) == Correct) ss))

penalties :: [Submission] -> Submission -> Int
penalties ss s = (length (filter (\x -> problem x == problem s && result x == Wrong) ss )) * 20


minutes (Submission m _ _) = m

problem (Submission _ p _) = p

result (Submission _ _ r) = r

writeOutput :: (Int, Int) -> String
writeOutput (a,b) = show a ++ " " ++ show b

