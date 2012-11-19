import System.IO

isEqualToSum n = n == sumDigits n

sumDigits :: Integer -> Integer
sumDigits 0 = 0
sumDigits n = m ^ 5 + sumDigits (n `div` 10)
  where m = n `mod` 10

main = do
  print $ show $ sum (filter isEqualToSum [2..194979])