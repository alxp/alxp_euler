import Factoring

hexagonal x = x * (2 * x - 1)

m = [x | x <- map hexagonal [144..], isTriangular x, isPentagonal x]

answer = head m

main = do
  print $ show answer