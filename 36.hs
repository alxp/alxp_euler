import Util

dec2bin :: Integer -> Integer
dec2bin 0 = 0
dec2bin x = x `mod` 2 + 10 * dec2bin (x `div` 2)

palindrome :: Integer -> Bool
palindrome x = show x == reverse (show x)

binaryPalindrome :: Integer -> Bool
binaryPalindrome x
  | even x = False
  | otherwise = palindrome x && palindrome (dec2bin x)
                
answer = sum [x | x <- [1..1000000], binaryPalindrome x]

main = do
  print $ show answer