{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import qualified Data.Text.IO as TI

main :: IO()
main = TI.interact (writeOutput . solve . readInput)

readInput :: T.Text -> [T.Text]
readInput = filter (/= "END") .  T.lines  

solve :: [T.Text] -> [Int]
solve = map (\s -> length (solve' s))

solve' :: T.Text -> [T.Text]
solve' "1" = ["1"]
solve' s = [s] ++ solve' (T.pack ( show (T.length s)))

writeOutput :: [Int] -> T.Text
writeOutput = T.unlines . map (T.pack . show)
