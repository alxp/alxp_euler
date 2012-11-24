module Permute where

permutes :: Eq a => [a] -> [[a]]
permutes (s: []) = [[s]]
permutes (s: ss) = [x : ys | (x,xs) <- selections (s:ss), ys <- permutes xs]

p :: Eq a => [a] -> Integer -> [[a]]
p (s: []) k
  | k == 0 = [[]]
  | otherwise = [[s]]
p (s: ss) k
  | k == 0 = [[]]
  | otherwise = [x : ys | (x, xs) <- selections (s:ss), ys <- p (xs) (k - 1)]

selections :: Eq a => [a] -> [(a,[a])]
selections xs = selections' xs xs

selections' :: Eq a => [a] -> [a] -> [(a,[a])]
selections' _ []= []
selections' whole (r: rs) = (r, filter (/= r) whole) : selections' whole rs
