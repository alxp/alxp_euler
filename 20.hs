import Util

myfactorial :: Integer
myfactorial = foldl (*) 100 [99,98..1]

total :: Integer
total = foldl (+) 0 (digits (myfactorial))

main = print $ show total