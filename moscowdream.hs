
main :: IO()
main = interact solve

solve :: String -> String
solve s = solve' a b c n 
  where input = map read (words s)
        a = input !! 0
        b = input !! 1
        c = input !! 2
        n = input !! 3

solve' :: Int -> Int -> Int -> Int -> String
solve' a b c n
  | a >= 1 && b >= 1 && c >= 1  && n >= 3 && (a + b + c) >= n = "YES"
  | otherwise = "NO" 
