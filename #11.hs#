import System.IO
import Control.Monad

numbers :: String -> [[Int]]
numbers c = (map (map readInt) (map words (lines c)))

readInt = read :: String -> Int

-- numberLine :: String -> [Int]
numberLine c = map (map readInt) (map words (lines c))

dropFirstColumn :: [[Int]] -> [[Int]]
dropFirstColumn nums = map (drop 1) nums

nthcolumn :: [[Int]] -> Int -> [Int]
nthcolumn nums n = map (head . (drop n)) nums

columns :: [[Int]] -> [[Int]]
columns ns
  | length (head ns) == 0 = []
  | otherwise = nthcolumn ns 0 : columns (dropFirstColumn ns)

diagonalDown :: [[Int]] -> [Int]
diagonalDown [[]] = []
diagonalDown [] = []
diagonalDown nums
  | length (head nums) == 0 = []
  | otherwise = head (head nums) : diagonalDown (dropFirstColumn (drop 1 nums))

diagonalsDown :: [[Int]] -> [[Int]]
diagonalsDown nums = diagonalRowsDown nums ++ diagonalColsDown nums

diagonalsUp :: [[Int]] -> [[Int]]
diagonalsUp nums = diagonalsDown (reverse nums)

diagonalRowsDown :: [[Int]] -> [[Int]]
diagonalRowsDown [] = []
diagonalRowsDown [[]] = []
diagonalRowsDown nums
  | length (head nums) == 0 = []
  | otherwise = diagonalDown nums : diagonalRowsDown (drop 1 nums)

diagonalColsDown :: [[Int]] -> [[Int]]
diagonalColsDown [[]] = []
diagonalColsDown [] = []
diagonalColsDown nums
  | length (head nums) == 0 = []
  | otherwise = diagonalDown nums : diagonalColsDown (dropFirstColumn nums)

highestProdOfRow :: Int -> [Int] -> Int
highestProdOfRow prev [] = prev
highestProdOfRow prev row = highestProdOfRow (max prev (prodOfRun4 row)) (drop 1 row)

prodOfRun4 :: [Int] -> Int
prodOfRun4 row | length row < 4 = 0
               | otherwise = row !! 0 * row !! 1 * row !! 2 * row !! 3

calc fname = do
  contents <- readFile fname
  let ns = numbers contents
  print $ maximum (map (highestProdOfRow 0) (concat [diagonalsUp ns, diagonalsDown ns, columns ns, ns]))

                            