onesToTeens :: Integer -> String
onesToTeens n
  | n == 0 = ""
  | n == 1 = "one"
  | n == 2 = "two"
  | n == 3 = "three"
  | n == 4 = "four"
  | n == 5 = "five"
  | n == 6 = "six"
  | n == 7 = "seven"
  | n == 8 = "eight"
  | n == 9 = "nine"
  | n == 10 = "ten"
  | n == 11 = "eleven"
  | n == 12 = "twelve"
  | n == 13 = "thirteen"
  | n == 14 = "fourteen"
  | n == 15 = "fifteen"
  | n == 16 = "sixteen"
  | n == 17 = "seventeen"
  | n == 18 = "eighteen"
  | n == 19 = "nineteen"

tens :: Integer -> String
tens n
  | b < 20 = onesToTeens b
  | b < 30 = "twenty" ++ rest
  | b < 40 = "thirty" ++ rest
  | b < 50 = "forty" ++ rest
  | b < 60 = "fifty" ++ rest
  | b < 70 = "sixty" ++ rest
  | b < 80 = "seventy" ++ rest
  | b < 90 = "eighty" ++ rest
  | b < 100 = "ninety" ++ rest
  | otherwise = ""
  where b = n `mod` 100
        c = n `mod` 10
        rest
          | c > 0 = "-" ++ onesToTeens c
          | otherwise = ""

hundreds :: Integer -> String
hundreds n
  | n < 100 = tens n
  | b == 0 = onesToTeens c ++ " hundred"
  | otherwise = onesToTeens c ++ " hundred and " ++ tens b
  where b = n `mod` 100
        c = n `div` 100

thousands :: Integer -> String
thousands n
  | n < 1000 = hundreds n
  | otherwise = onesToTeens c ++ " thousand " ++ hundreds b
  where b = n `mod` 1000
        c = n `div` 1000

countLetters :: String -> Integer
countLetters [] = 0
countLetters (x: xs)
  | elem x " -" = countLetters xs
  | otherwise = 1 + countLetters xs