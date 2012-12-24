import System.IO
import Control.Monad
import Data.List
import Util

findFirstRepetition :: String -> Maybe Int
findFirstRepetition num
  | otherwise = Just (length (takeWhile (== Nothing) repeatList) + 1)
  where repeatList = [findRepetitionFor x 0 num | x <- [1..200]]
    
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
  print $ show $ elemIndex maxx x where 
    x = [findFirstRepetition (show (10^2000 `div` x)) | x <- [1..1000]]
    maxx = maximum x