module Factoring where

import Util
import Data.List

combinations :: [Integer] -> [[Integer]]                    
combinations [] = [[]]
combinations (xs: xss) = [x:y | x <- [xs], y <- combinations xss] ++ combinations xss

prod :: [Integer] -> Integer
prod [] = 1
prod (x: xs) = x * prod xs

allFactors :: Integer -> [Integer]
allFactors 0 = []
allFactors 1 = [1]
allFactors n = nub $ map prod (combinations (factor n))

triangles :: [Integer]
triangles = triangles' 1 0

triangles' :: Integer -> Integer -> [Integer]
triangles' step prev = next : (triangles' (step + 1) next)
  where next = prev + step

triangular :: Integer -> Bool
triangular n = triangular' 0 n

triangular' :: Integer -> Integer -> Bool
triangular' i n
  | triangles !! (fromIntegral i) == n = True
  | triangles !! (fromIntegral i) > n = False
  | otherwise = triangular' (i + 1) n
                
isPentagonal :: Integer -> Bool
isPentagonal y = x - (fromIntegral (floor x)) == 0.0
  where x = ((sqrt $ fromIntegral (24 * y + 1)) + 1) / 6                
        
isTriangular :: Integer -> Bool
isTriangular y = fromIntegral (floor x) - x == 0
  where x =((sqrt (8 * (fromIntegral y) + 1) - 1)::Double) / 2 
        
isHexagonal :: Integer -> Bool
isHexagonal y = fromIntegral (floor x) - x == 0
  where x =((sqrt (8 * (fromIntegral y) + 1) + 1)::Double) / 4 

