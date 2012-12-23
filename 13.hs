import System.IO
import Control.Monad

numbers :: String -> [Integer]
numbers c = (map readInt) (lines c)

readInt = read :: String -> Integer

calc fname = do
  contents <- readFile fname
  let ns = numbers contents
  print $ take 10 $ show $ sum ns

main = calc "13.txt"                            