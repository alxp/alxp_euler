
nextInSeq :: Int -> Int
nextInSeq n
  | even n = n `div` 2
  | otherwise = 3 * n + 1

recursiveSequence :: Int -> [Int]
recursiveSequence n
  | n == 1 = [1]
  | otherwise = n : recursiveSequence (nextInSeq n)

lengthChecker :: Int -> Int -> Int
lengthChecker highest next
  | next == 1 = highest
  | length (recursiveSequence next) > length (recursiveSequence highest) = lengthChecker next (next - 1)
  | otherwise = lengthChecker highest (next - 1)