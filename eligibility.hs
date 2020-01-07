
type Name = String
type StudiesStarted = Date
type Birth = Date
type Courses = Int
type Student = (Name, StudiesStarted, Birth, Courses)
  

main :: IO()
main = interact (solve . readInput)

readInput :: String -> [Student]

solve :: [Student] -> String

