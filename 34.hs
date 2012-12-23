import Util


digitsMap = zip [1..] (map factorial [1..9])

sumDigitFacts :: Integer -> Integer
sumDigitFacts n = sum (map factorial (digits n))

main = print $ show $ sum $ [x | x <- [1..999999], x == sumDigitFacts x]