{-# LANGUAGE BangPatterns #-}
module Fib where

fib :: Integer -> Integer
fib n = go n (0, 1)
  where
    go !n (!a, !b) | n == 0 = a
                   | otherwise = go (n-1) (b, a+b)

fibsBelow :: Integer -> [Integer]
fibsBelow limit = go 0 limit
  where
    go n limit | fib n > limit = []
               | otherwise = [fib n] ++ (go (n + 1) limit)

total = sum [j | j <- fibsBelow 4000000, j `mod` 2 == 0]