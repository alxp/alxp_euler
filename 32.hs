import Data.List
import Data.Set
import Permute

digits :: Integer -> [Integer]
digits 0 = []
digits i = (i `mod` 10) : (digits (i `div` 10))

numDigits :: Integer -> Integer
numDigits n = 1 + floor(logBase 10 (fromIntegral n))

pandigital' :: [Integer] -> Bool
pandigital' n = fromList n == fromList [1..(fromIntegral d)]
  where d = length n

pandigital :: Integer -> Bool
pandigital n = fromList (digits n) == fromList [1..d]
  where d = numDigits n

lton :: [Integer] -> Integer
lton [] = 0
lton (x:xs) = 10 ^ (length (x:xs) - 1) * x + lton xs


ans2 = [(lton (take 1 n), lton (drop 1 n)) | n <- l]

l = p [1..9] 5

ans = [(lton (take 2 n), lton (drop 2 n)) | n <- l]

main = do
  print $ show $ ( sum (nub [(x*y) | (x,y) <- ans ++ ans2, pandigital' (digits x ++ digits y ++ (digits (x * y))) ]))