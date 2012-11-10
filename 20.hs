import Util (digits)

factorial :: Integer
factorial = foldl (*) 100 [99,98..1]

total :: Integer
total = foldl (+) 0 (digits (show factorial))