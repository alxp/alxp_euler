import System.IO
import Control.Monad
import Data.List
import Data.Char
import Factoring

-- Grabbed from http://stackoverflow.com/a/4981265/19513                                                        
wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
  "" -> []
  s' -> w : wordsWhen p s''
    where (w, s'') = break p s'

nameScore :: String -> Integer
nameScore [] = 0
nameScore (s: ss) = (fromIntegral (ord s)) - 64 + nameScore ss

nameScores :: Integer  -> [String] -> Integer
nameScores _ [] = 0
nameScores c (s: ss) = c * nameScore s + nameScores (c + 1) ss

triangularNameScore :: Integer -> [String] -> Integer
triangularNameScore prev [] = prev
triangularNameScore prev (s: ss)
  | triangular (nameScore s) = triangularNameScore (1 + prev) ss
  | otherwise = triangularNameScore prev ss

calc fname = do
  contents <- readFile fname
  let ns = sort $ wordsWhen (flip elem [',', '"']) contents
  print $ show $ triangularNameScore 0 ns

main = calc "words.txt"


