import System.IO
import Permute

digits = ['0','1','2','3','4','5','6','7','8','9']

millionth = head (drop 999999 (permutes digits))

main = do
  print $ show $ millionth