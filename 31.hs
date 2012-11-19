import System.IO

allCoins = [200, 100, 50, 20, 10, 5, 2, 1]

type Count = Integer
type Value = Integer

type Coin = (Count, Value)

coinsFor :: Value -> [Value] -> [[Coin]]
coinsFor total (singleDenom : [])
  | total `mod` singleDenom == 0 = [[(total `div` singleDenom, singleDenom)]]
  | otherwise = []
coinsFor total (denom: rest) = (countCoins total (0, denom) rest)

countCoins :: Value -> Coin -> [Value] -> [[Coin]]
countCoins total (count, denom) rest
  | count * denom > total = []
  | otherwise = (map ((count, denom) : ) (coinsFor (total - count * denom) rest)) ++ countCoins total (count + 1, denom) rest

main = do
  print $ show $ length $ coinsFor 200 allCoins