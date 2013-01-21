import Util
import Data.List

type Triangle = (Integer, Integer)

isSquare x = elem x (takeWhile (<= x) [x^2 | x <- [1..]])

evenRight :: Triangle -> Bool
evenRight (l, h) = isSquare (l ^ 2 + h ^ 2)

evenTriangle :: Integer -> Integer -> Bool
evenTriangle a b = isSquare (a^2 + b^2)

s :: Integer -> Integer -> Integer
s a b = (a + b + floor x)
  where x = (sqrt (fromIntegral (a^2 + b^2)))

s' :: Integer -> Integer -> Maybe Integer
s' a b
  | x - fromIntegral (floor x) == 0 = Just (a + b + floor x)
  | otherwise = Nothing
  where x = (sqrt (fromIntegral (a^2 + b^2)))
        
triangles = nub [((s a b), a, b) | a <- [1..500], b <- [1..500], s' a b /= Nothing, s' a b <= Just 1000, b >= a]

fst3 :: (a, a, a) -> a
fst3 (x, _, _) = x

frequency :: Ord a => [(a, a, a)] -> [(Int,a)] 
frequency x = map (\l -> (length l, head l)) (group (sort list))
  where list = fst3 (unzip3 x)
        
answer = last $ sort $ frequency triangles

main = do
  print $ show answer