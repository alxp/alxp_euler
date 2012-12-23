import Util
import Data.Array.Unboxed

oneIfPrime :: Integer -> Int
oneIfPrime x
  | isPrime x = 1
  | otherwise = 0

allPrimes :: UArray Integer Bool
allPrimes = array (1,1000000) [(x, isPrime x) | x <- [1..1000000]]

isPrime' :: Integer -> Bool
isPrime' x = allPrimes ! x

rotate :: Integer -> Integer
rotate x = end * 10 ^ d + start where
  end = x `mod` 10
  start = x `div` 10
  d = (numDigits x) - 1

rotateN :: Integer -> Integer -> Integer
rotateN 0 x = x
rotateN n x = rotateN (n - 1) (rotate x)

-- Check that all of the rotations of digits of x are prime.
rotationsPrime :: Integer -> Bool
rotationsPrime x = rotationsPrime' (numDigits x) x

rotationsPrime' :: Integer -> Integer -> Bool
rotationsPrime' n x
  | n == 0 = True
  | not (isPrime' x) = False
  | otherwise = rotationsPrime' (n - 1) (rotate x)

main = do
  print $ show $ length $ [x | x <- primesToGT 1000000, rotationsPrime x]