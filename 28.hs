import System.IO

chomp :: Int -> Int -> [Int] -> [Int]
chomp count thisInterval xs
  | count > 4 = chomp 1 (thisInterval + 2) xs
  | otherwise = head (drop thisInterval xs) : chomp (count + 1) thisInterval (drop (thisInterval + 1) xs)

main = do
  print $ show $ 1 + sum (take 2000 $ chomp 1 1 [2..])