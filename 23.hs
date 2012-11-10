import Factoring
import System.IO

perfect :: Integer -> Bool
perfect n = sum (drop 1 $ allFactors n) == n

abundant :: Integer -> Bool
abundant n = sum (drop 1 $ allFactors n) > n

abundants = filter abundant [1..28123]

abundantsBelow :: Integer -> [Integer]
abundantsBelow n = takeWhile (< n) abundants

summable' :: Integer -> Integer -> [Integer] -> Bool
summable' n x [] = n == x + x
--summable' n x (x': xs) = n == x + x || n == x + x' || summable' n x xs


summable :: Integer -> [Integer] -> Bool
summable _ [] = False
summable n (x: xs)
  | x + x == n = True
  | abundant (n - x) == True = True
  | x > n = False
  | otherwise = summable n xs

abundantsSummable :: Integer -> Bool
abundantsSummable n = summable n (abundantsBelow n)

notAbundantsSummable :: Integer -> Bool
notAbundantsSummable n = not (abundantsSummable n)

main = do
  print $ show $ sum $ filter (notAbundantsSummable) [1..29000]