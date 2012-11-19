import Util

howManyPrimes :: [Integer] -> Integer
howManyPrimes xs = howManyPrimes' 0 xs

howManyPrimes' :: Integer -> [Integer] -> Integer
howManyPrimes' c (x: xs)
  | isPrime x = 1 + (howManyPrimes' (c + 1) xs)
  | otherwise = 0

f :: Integer -> Integer -> Integer -> Integer
f n a b = n^2 + a * n + b

lengthOfPrimeRun :: Integer -> Integer -> (Integer, Integer, Integer)
lengthOfPrimeRun a b = (fromIntegral $ length $ takeWhile (== True) [isPrime (f x a b)| x <- [0..]], a, b)

maxRun :: (Integer, Integer, Integer) -> (Integer, Integer, Integer) -> (Integer, Integer, Integer)
maxRun (a, b, c) (a', b', c')
  | a > a' = (a, b, c)
  | otherwise = (a', b', c')

