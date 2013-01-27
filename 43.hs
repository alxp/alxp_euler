import Permute
import Util

x = [cat x | x <- permutes [0..9], cat x > 1000000000]

d i n = cat [digs i, digs (i + 1), digs (i +2)]
  where digs j = reverse (digits n) !! (j - 1)
        
babyPrimes = [2, 3, 5, 7, 11, 13, 17, 19, 23]

divisible n = and [d i n `mod` (babyPrimes !! (i - 2)) == 0 | i <- [2..8]]

answer = sum [m | m <- x, divisible m]

main = do 
  print $ show answer