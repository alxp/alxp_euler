import Fib
import System.IO

lowestLessThanOneThousand = head (dropWhile (< 10^999) (map fib [1..]))

main = do
  print $ show lowestLessThanOneThousand