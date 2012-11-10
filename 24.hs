import System.IO

digits = ['0','1','2','3','4','5','6','7','8','9']

permutes :: String -> [String]
permutes (s: []) = [[s]]
permutes (s: ss) = [x : ys | (x,xs) <- selections (s:ss), ys <- permutes xs]


selections :: String -> [(Char,String)]
selections xs = selections' xs xs

selections' :: String -> String -> [(Char,String)]
selections' _ []= []
selections' whole (r: rs) = (r, filter (/= r) whole) : selections' whole rs

millionth = head (drop 999999 (permutes digits))

main = do
  print $ show $ millionth