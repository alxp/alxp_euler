import Util
import Data.List

combinations :: [Integer] -> [[Integer]]                    
combinations [] = [[]]
combinations (xs: xss) = [x:y | x <- [xs], y <- combinations xss] ++ combinations xss

prod :: [Integer] -> Integer
prod [] = 1
prod (x: xs) = x * prod xs

allFactors :: Integer -> [Integer]
allFactors 1 = [1]
allFactors n = nub $ map prod (combinations (factor n))

triangles :: [Integer]
triangles = triangles' 1 0

triangles' :: Integer -> Integer -> [Integer]
triangles' step prev = next : (triangles' (step + 1) next)
  where next = prev + step

main = do
  print $ show $take 1 [x | x <- triangles, length (allFactors x) > 500]