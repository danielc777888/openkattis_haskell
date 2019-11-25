import Data.Char

shorten :: String -> String
shorten [] = []
shorten xs = filter isUpper xs

main :: IO()
main = do
  authors <- getLine
  putStrLn (shorten authors)
