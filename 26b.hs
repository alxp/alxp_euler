import Util
import Data.List
import Control.Applicative

check :: Integer -> Integer
check n = 10 ^ (10 + numDigits n) `mod` n

checkRepeat :: Integer -> Integer -> Integer -> Integer -> Maybe Integer
checkRepeat n r p i 
  | r' n i == 0 = Nothing  
  | r' n i == p = Just 1
  | r' n i == r = Just i
  | otherwise = checkRepeat n r (r' n i) (i + 1)

        
r' n i = 10 ^ (10 + (numDigits n) + i) `mod` n


-- Used to add 1 to a number of the form "Just 3"
add1 n = (+1) <$> n

myseq = [checkRepeat x (check x) (check x) 1 | x <- [1..1000]]

main = 
  print $ show $ add1 $ elemIndex (maximum myseq) myseq