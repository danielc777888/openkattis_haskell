{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import qualified Data.String as S

readInput :: String -> [String]
readInput = S.lines

solve :: [String] -> Int
solve xs = read (T.unpack (  ((T.splitOn " " (T.pack (head xs))) !! 1)))


writeOutput :: Int -> String
writeOutput  = show


main :: IO()
main = interact (writeOutput . solve . readInput)
