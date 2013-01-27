import Util

numberAtPlace :: Integer -> (Integer, Integer)
numberAtPlace n = (10^((dforn n) - 1) + (n - base) `div` (dforn n), (n - base) `mod` (dforn n))
  where base = baseforn n

d x = (reverse $ digits m) !! (fromIntegral n)
  where (m, n) = numberAtPlace x

-- At what position do the numbers with same digits as n start?
baseforn n = last (takeWhile (< n) [nford x | x <- [1..]])

-- At what place do the d-digit numbers begin?
nford d = d * 10 ^ d - sum [10 ^ x | x <- [d-1,d-2..1]]

-- How many digits are the numbers at position n?
dforn n = head (dropWhile (\x -> nford x < n) [x | x <- [1..]])

answer = 1 * d 100 * d 1000 * d 10000 * d 100000 * d 1000000
 
main = print $ show answer