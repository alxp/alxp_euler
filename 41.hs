import Util
import Permute

x n = [cat x | x <- permutes [1..n], isPrime (cat x)]

cat x = foldl concatDigits' 0 x

answer = last (x 7)

main = do
  print $ show answer