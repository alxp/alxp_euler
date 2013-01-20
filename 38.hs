import Util

mul123 a = (1 * a) * 10^6 + (2 * a) * 10^3 + (3 * a) * 10^0

concatDigits n m = m * 10 ^ (numDigits n) + n

mul1tox :: Integer -> Integer -> Integer
mul1tox n d
  | d == 0 = 0
  | otherwise = concatDigits (d * n) (mul1tox n (d - 1))

hasDigit :: Integer -> Integer -> Bool
hasDigit n d
  | n == 0 = False
  | n `mod` 10 == d = True
  | otherwise = hasDigit (n `div` 10) d
                
panDigital n = and [hasDigit n d | d <- [1..9]]

panDigitals :: Integer -> Integer -> [Integer]
panDigitals n d
  | numDigits x < 9 = panDigitals (n + 1) d
  | numDigits x > 9 = []
  | panDigital x = (x : rest)
  | otherwise = rest
  where x = mul1tox n d
        rest = panDigitals (n + 1) d

answer = maximum $ concat $ [panDigitals 1 n | n <- [2..100]]

main = do
  print $ show $ answer