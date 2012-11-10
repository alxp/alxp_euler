import Factoring
import System.IO

factorSum :: Integer -> Integer
factorSum n = sum $ drop 1 $ allFactors n

doubleFactorSum :: Integer -> Integer
doubleFactorSum n = iterate factorSum n !! 2

isAmicable :: Integer -> Bool
isAmicable n = n == doubleFactorSum n && n /= (factorSum n)

main = do
  print $ show $ sum (filter isAmicable [1..10000])