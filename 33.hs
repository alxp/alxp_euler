import Factoring
import Data.List

-- After finishing this I realize I could hav ejust simply run through the
-- whole set of number 11-99 / 11-99 in less code but I sitll like
-- taking advantage of declaring an instance of (*) which gives you
-- associativity and lets me do foldl on fraction multiplication.

data Fraction = Fraction (Integer, Integer)
              deriving Show

instance Eq Fraction where
  Fraction (n1, d1) == Fraction (n2, d2) = a == b where
    a = fromIntegral n1 / fromIntegral d1
    b = fromIntegral n2 / fromIntegral d2

instance Num Fraction where
  Fraction (n1, d1) * Fraction (n2, d2) = reduce (Fraction (n1 * n2, d1 * d2))

commonFactors :: Fraction -> [Integer]
commonFactors (Fraction (n, d)) = filter ((/=) 1) $ intersect (allFactors n) (allFactors d)

reduce :: Fraction -> Fraction
reduce (Fraction (n, d))
  | commonFactors (Fraction (n, d)) == [] = (Fraction (n, d))
  | otherwise = (Fraction (n `div` c, d `div` c)) where
    c = head (commonFactors (Fraction (n, d)))

generateTensComponents :: Integer -> [Integer]
generateTensComponents n
  | n >= 10 || n <= 0 = []
  | otherwise = [10 * x + n | x <- [1..9], x /= n]

generateOnesComponents :: Integer -> [Integer]
generateOnesComponents n
  | n >= 10 || n <= 0 = []
  | otherwise = [10 * n + x | x <- [1..9], x /= n]

curious :: Fraction -> Bool
curious (Fraction (n, d)) = undefined

findCurious :: Integer -> [Fraction]
findCurious m = [Fraction (n, d) | n <- generateTensComponents m, d <- generateOnesComponents m, Fraction (n, d) == Fraction ( removeDigit n m, removeDigit d m)]

allCurious = filter ((/=) []) $ map findCurious [1..9]

removeDigit :: Integer -> Integer -> Integer
removeDigit x d
  | x `div` 10 == d = x `mod` 10
  | x `mod` 10 == d = x `div` 10                      

numerator :: Fraction -> Integer
numerator (Fraction (n, _)) = n

denominator :: Fraction -> Integer
denominator (Fraction (_, d)) = d

answer = denominator (foldl (*) (Fraction (1, 1)) (concat allCurious))

main :: IO ()
main = do 
  print $ show answer