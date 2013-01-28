import Util
import Data.List

test n d = and [length (nub $ factor m) >= fromIntegral d | m <- [n..n+d-1]]

answer = head [m | m <- [644..], test m 4 == True]

main = print $ show answer