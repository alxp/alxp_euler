import Util

answer :: Integer
answer = factorial 40 `div` ((factorial 20) * (factorial 20))

main = print $ show $ answer