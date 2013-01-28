import Util

squares = [x^2 | x <- [1..]]

candidateSquares n = takeWhile (< n `div` 2) squares
                                                    
                                                    
diffs n = map ((n -) . (*2)) (candidateSquares n)

test n = or (map isPrime (diffs n))

answer = head [x | x <- [3,5..], test x == False, not (isPrime x)]

main = do
  print $ show answer