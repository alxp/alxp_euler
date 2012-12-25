import Util
import Data.Array.Unboxed

allPrimes :: UArray Integer Bool
allPrimes = array (1,1000000) [(x, isPrime x) | x <- [1..1000000]]

isPrime' :: Integer -> Bool
isPrime' x = allPrimes ! x

truncatableL :: Integer -> Bool
truncatableL n 
  | n == 0 = True
  | otherwise = isPrime' n && truncatableL (n `mod` (10 ^ ((numDigits n) - 1)))

truncatableR :: Integer -> Bool
truncatableR n
  | n == 0 = True
  | otherwise = isPrime' n && truncatableR (n `div` 10)
                
answer = sum $ take 11 [x | x <- [10..], truncatableR x, truncatableL x]                
main = print $ show answer