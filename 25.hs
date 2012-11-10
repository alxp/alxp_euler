import Fib

lowestLessThanOneThousand = head (dropWhile (< 10^999) (map fib [1..]))