import Data.List

main :: IO()
main = interact (writeOutput . solve . readInput)

zero :: [String]
zero = [ "xxxxx"
       , "x...x"
       , "x...x"
       , "x...x"
       , "x...x"
       , "x...x"
       , "xxxxx" ]

one :: [String]
one = [ "....x"
      , "....x"
      , "....x"
      , "....x"
      , "....x"
      , "....x"
      , "....x" ]

two :: [String]
two = [ "xxxxx"
      , "....x"
      , "....x"
      , "xxxxx"
      , "x...."
      , "x...."
      , "xxxxx" ]

three :: [String]
three = [ "xxxxx"
        , "....x"
        , "....x"
        , "xxxxx"
        , "....x"
        , "....x"
        , "xxxxx" ]

four :: [String]
four = [ "x...x"
       , "x...x"
       , "x...x"
       , "xxxxx"
       , "....x"
       , "....x"
       , "....x" ]

five :: [String]
five = [ "xxxxx"
       , "x...."
       , "x...."
       , "xxxxx"
       , "....x"
       , "....x"
       , "xxxxx" ]

six :: [String]
six = [ "xxxxx"
      , "x...."
      , "x...."
      , "xxxxx"
      , "x...x"
      , "x...x"
      , "xxxxx" ]

seven :: [String]
seven = [ "xxxxx"
        , "....x"
        , "....x"
        , "....x"
        , "....x"
        , "....x"
        , "....x" ]

eight :: [String]
eight = [ "xxxxx"
        , "x...x"
        , "x...x"
        , "xxxxx"
        , "x...x"
        , "x...x"
        , "xxxxx" ]


nine :: [String]
nine = [ "xxxxx"
       , "x...x"
       , "x...x"
       , "xxxxx"
       , "....x"
       , "....x"
       , "xxxxx" ]


readInput :: String -> (Int, Int)
readInput s = (num1, num2)
              where transposed = transposeAscii ( map readRow (lines s))
                    ints = map  toInt transposed
                    num1 = fromList $ takeWhile (/= -1) ints
                    num2 = fromList $ tail (dropWhile (/= -1) ints)

readRow :: String -> [String]
readRow [] = []
readRow xs = [take 5 xs] ++ readRow (drop 6 xs) 

fromList :: [Int] -> Int
fromList xs = foldl makeNum 0 xs
              where makeNum num d = num * 10 + d

toInt :: [String] -> Int
toInt xs | xs == zero  = 0
               | xs == one  = 1
               | xs == two  = 2
               | xs == three  = 3
               | xs == four  = 4
               | xs == five  = 5
               | xs == six  = 6
               | xs == seven  = 7
               | xs == eight  = 8
               | xs == nine  = 9
               | otherwise = -1

toAscii :: Char -> [String]
toAscii xs | xs == '0' = zero
               | xs == '1' = one
               | xs == '2' = two
               | xs == '3' = three
               | xs == '4' = four
               | xs == '5' = five
               | xs == '6' = six
               | xs == '7' = seven
               | xs == '8' = eight
               | xs == '9' = nine
               | otherwise = zero

transposeAscii :: [[String]] -> [[String]]
transposeAscii ([]:_) = []
transposeAscii xs = (map head xs) : (transposeAscii (map tail xs))

solve :: (Int, Int) -> Int
solve  (a,b) =  a + b

writeOutput :: Int -> String
writeOutput x =  unlines $ map (intercalate ".") asc
                where asc = transposeAscii $ map toAscii (show x)
 
