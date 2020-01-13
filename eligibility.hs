import Data.Time

type Name = String
type StudiesStarted = Day
type Birth = Day
type Courses = Integer
type Student = (Name, StudiesStarted, Birth, Courses)
  

main :: IO()
main = interact (solve . readInput)

readInput :: String -> [Student]
readInput s = map (\x -> (x !! 0, parseDay (x !! 1), parseDay (x !! 2), read (x !! 3)))  (map words l)
  where l = tail (lines s)

parseDay :: String -> Day
parseDay s = parseTimeOrError True defaultTimeLocale "%Y/%m/%d" s :: Day

solve :: [Student] -> String
solve ss = unlines (map (\(n,s,b,c) -> n  ++ " " ++ eligibility (year s) (year b) c) ss)

year :: Day -> Integer
year d = y
  where (y, _, _) = toGregorian d

eligibility :: Integer -> Integer -> Courses -> String
eligibility s b c
  | s >= 2010 = "eligible"
  | b >= 1991 = "eligible"
  | (fromIntegral c / 5.0) > 8.0 = "ineligible"
  | (fromIntegral c / 5.0) <= 8.0 = "coach petitions"
  | otherwise = "ineligible"   
