isPrime :: Int -> Bool
isPrime n = length [j | j <- upToSqrt n, n `mod` j == 0] == 0

upToSqrt :: Int -> [Int]
upToSqrt n = intsBelow 1 n

intsBelow :: Int -> Int -> [Int]
intsBelow c n | c > round (sqrt (fromIntegral n)) = []
              | otherwise = c+1 : intsBelow (c+1) n
                            
answer = drop 9999 $ take 10000 [n | n <- [1..], isPrime n]

main = do
  print $ show answer