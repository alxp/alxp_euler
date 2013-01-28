import Util
import Data.List

m = [x | x <- primesToGT 10000]

sameDigits x y = sort (digits x) == sort (digits y) 

check x = [(x, y, x + (y - x) * 2) | y <- m, y > x, isPrime (x + (y - x) * 2), sameDigits x y, sameDigits x (x + (y - x) * 2)]

candidates = last [check x | x <- primesToGT 10000,check x /= []]

answer (a, b, c) = a * 100000000 + b * 10000 + c

main = print $ show $ answer (last candidates)