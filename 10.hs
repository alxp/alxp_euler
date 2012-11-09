isPrimeOld :: Int -> Bool
isPrimeOld n = length [j | j <- upToSqrt n, n `mod` j == 0] == 0

upToSqrt :: Int -> [Int]
upToSqrt n = intsBelow 1 n

intsBelow :: Int -> Int -> [Int]
intsBelow c n | c > round (sqrt (fromIntegral n)) = []
              | otherwise = c+1 : intsBelow (c+1) n


primesToGT m = 2 : sieve [3,5..m]  where
  sieve (p:xs)
    | p*p > m = p : xs
    | True    = p : sieve [x | x <- xs, rem x p /= 0]