import Data.List
import System.IO

answer :: Int
answer = length (nub (sort [x ^ y | x <- [2..100], y <- [2..100]]))

main = do
  print $ show answer