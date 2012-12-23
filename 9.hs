isInt :: Float -> Bool
isInt x = x == fromInteger (round x)

hyp :: Integer -> Integer -> Float
hyp x y = sqrt (x' ^ 2 + y' ^ 2)
  where x' = fromIntegral x
        y' = fromIntegral y

factors = [(x, y) | x <- [1..500], y <- [1..500], isInt (hyp x y), (fromIntegral x) + (fromIntegral y) + (hyp x y) == 1000.0]

prod = x * y * floor (hyp x y)
  where (x, y) = head factors
        
main = do
  print prod