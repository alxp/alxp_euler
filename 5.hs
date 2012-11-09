p :: Integer
p = 2432902008176640000

primes :: [Integer]
primes = [19, 17, 13, 11, 7, 5, 3, 2]

fullyDivisible :: Integer -> Bool
fullyDivisible m = and [m `mod` o == 0 | o <- [1..20]]

-- Take a number and the remaining list of primes, and divide by the largest
-- prime until we get to an empty list.
next :: Integer -> [Integer] -> Integer
next n ps
  | fullyDivisible (n `div` (head ps)) = next (n `div` (head ps)) ps
  | length ps == 1 = n
  | otherwise = next n (drop 1 ps)



