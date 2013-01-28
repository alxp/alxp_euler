

pentagonal n = n * (3 * n - 1) `div` 2

pentagonals = [pentagonal n | n <- [1..]]

isPentagonal :: Integer -> Bool
isPentagonal n = isPentagonal' 0 n

isPentagonal'' :: Integer -> Bool
isPentagonal'' y = x - (fromIntegral (floor x)) == 0.0
  where x = ((sqrt $ fromIntegral (24 * y + 1)) + 1) / 6

isPentagonal' :: Integer -> Integer -> Bool
isPentagonal' i n
  | pentagonals !! (fromIntegral i) == n = True
  | pentagonals !! (fromIntegral i) > n = False
  | otherwise = isPentagonal' (i + 1) n
                
answer = head [(x, y) | x <- take 100000 pentagonals, y <- take 100000 pentagonals, x > y, 
               isPentagonal'' (x + y), isPentagonal'' (x - y)]