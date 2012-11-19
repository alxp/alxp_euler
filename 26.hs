import System.IO
import Control.Monad
import Util

data DFrame = DFrame
              {
                divisor :: Integer
              , dividend :: Integer
              , quotient :: Integer
              , remainder :: Integer
              } deriving (Show)

readInt = read :: String -> Integer

getDividend :: Integer -> Integer -> Integer
getDividend divisor dividend = head (dropWhile (\x -> x `div` divisor == 0) (digits dividend))

divStep :: DFrame -> DFrame
divStep d = DFrame { divisor = divisor d
                   , dividend = dividend d `div` divPortion
                   , quotient = divPortion `div` divisor d
                   , remainder = divPortion `mod` divisor d}
  where divPortion = getDividend (divisor d) (dividend d)        

digits :: Integer -> [Integer]
digits n = digits' s 1
  where s = show n


digits' :: String -> Integer -> [Integer]
digits' num pos
  | length num < fromIntegral pos = (readInt num * 10 : digits' (num ++ "0") (pos + 1))
  | otherwise = readInt (take (fromIntegral pos) num) : digits' num (pos + 1)

findFirstRepetition :: String -> Maybe Int
findFirstRepetition num
  | otherwise = Just (length (takeWhile (== Nothing) repeatList) + 1)
  where repeatList = [findRepetitionFor x 0 num | x <- [1..2000]]
    
findRepetitionFor :: Int -> Int -> String -> Maybe Int
findRepetitionFor len start num
  -- We've run out of numbers at this try
  | len >= fromIntegral (length num) `div` 2 = Nothing
  | repeats True (take len num) num == True = Just start
  | otherwise = findRepetitionFor len (start + 1) (drop 1 num)

repeats :: Bool -> String -> String -> Bool
repeats firstCall test num
  | length test > nlen = take nlen test == num && firstCall == False
  | otherwise = take tlen num == test && repeats False test (drop tlen num)
  where tlen = length test
        nlen = length num

main = do
  print $ show $ foldl max (Just 0) [findFirstRepetition (show (10^2000 `div` x)) | x <- filter isPrime [1..1000]]