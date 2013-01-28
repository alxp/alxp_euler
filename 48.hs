import Util

x = sum [x ^ x | x <- [1..1000]]

answer = x `mod` 10000000000

main = print $ show answer