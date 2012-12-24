import Fib
import System.IO

lowestLessThanOneThousand = head (dropWhile (\y -> snd y < 10^999) (zip [1..] (map fib [1..]))) 

main = do
  print $ show lowestLessThanOneThousand