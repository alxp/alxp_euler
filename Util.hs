{-# LANGUAGE BangPatterns #-}
module Util where

import Data.Array.Unboxed

isPrime :: Integer -> Bool
isPrime 2 = True
isPrime n
  | n < 2 = False
  | otherwise = length [j | j <- upToSqrt n, n `mod` j == 0] == 0
--isPrime n = n == head (drop (length ps - 1) (ps))
--  where ps = primesToGT n

inPrimes :: Integer -> [Integer] -> Bool
inPrimes _ [] = False
inPrimes n (p: ps)
  | n == p = True
  | otherwise = inPrimes n ps

factor :: Integer -> [Integer]
factor x | isPrime x = [x]
         | otherwise = (m : factor (x `div` m))
  where
    m = head [j| j <- (upToSqrt x), x `mod` j == 0]

upToSqrt :: Integer -> [Integer]
upToSqrt n = primesToGT (floor (sqrt (fromIntegral n)))

intsBelow :: Integer -> Integer -> [Integer]
intsBelow c n | c > round (sqrt (fromIntegral n)) = []
              | otherwise = c+1 : intsBelow (c+1) n                            

primesToGT m = 2 : sieve [3,5..m]  where
  sieve [] = []
  sieve (p:xs)
    | p*p > m = p : xs
    | True    = p : sieve [x | x <- xs, rem x p /= 0]
                
digits :: Integer -> [Integer]
digits 0 = []
digits i = (i `mod` 10) : (digits (i `div` 10))

factorial :: Integer -> Integer
factorial x = foldl (*) 1 [1..x]
                
numDigits :: Integer -> Integer
numDigits x
  | x < 0 = numDigits (abs x)
  | x < 10 = 1
  | otherwise = 1 + numDigits (x `div` 10)
                
concatDigits n m = m * 10 ^ (numDigits n) + n              

concatDigits' m n = concatDigits n m