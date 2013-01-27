import Data.List

{- Calculate the perimeter of a right triangle with legs a and b, but only if
   the triangle's sides are all integers. Nothing otherwise.
-}
s' :: Integer -> Integer -> Maybe Integer
s' a b
  | size - fromIntegral (floor size) == 0 = Just (floor size)
  | otherwise = Nothing
  -- size uses the Pythagorean theorem to solve for the hypoteneuse.
  where size = sqrt (fromIntegral (a^2 + b^2)) + fromIntegral(a + b)
        
{- Generate a list of triangles with 3 whole number sides. We only want triangles with
   perimeter <= 1000, so we can assume no single side will be > 1000. We eliminate
   duplicates by first not repeating ourselves by ensuring b >= a, while using nub
   to catch the special case of a == b.
-}
triangles = nub [((s' a b), a, b) 
                | a <- [1..500], b <- [a..500]
                  , s' a b /= Nothing, s' a b <= Just 1000, a + b < 500]

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

-- Discard the triangles' legs and just keep the perimeters
perims :: Ord a => [(Maybe a, b, b)] -> [Maybe a]
perims x = fst3 $ unzip3 x

{- frequency function taken from this Stack Overflow answer
   http://stackoverflow.com/a/10398926/19513
-}
frequency :: Ord a => [(Maybe a, b, b)] -> [(Int, Maybe a)] 
frequency x = map (\l -> (length l, head l)) (group (sort list))
  where list = perims x
        
answer = last $ sort $ frequency triangles

main = do
  print $ show answer