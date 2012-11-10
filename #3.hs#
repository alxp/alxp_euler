{-# LANGUAGE BangPatterns #-}
module Three where

isPrime :: Int -> Bool
isPrime n = length [j | j <- upToSqrt n, n `mod` j == 0] == 0

factor :: Int -> [Int]
factor x | isPrime x = [x]
         | otherwise = (m : factor (x `div` m))
  where
    m = head [j| j <- (upToSqrt x), x `mod` j == 0]

upToSqrt :: Int -> [Int]
upToSqrt n = intsBelow 1 n

intsBelow :: Int -> Int -> [Int]
intsBelow c n | c > round (sqrt (fromIntegral n)) = []
              | otherwise = c+1 : intsBelow (c+1) n

q3 = factor 600851475143