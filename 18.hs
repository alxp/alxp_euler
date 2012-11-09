import System.IO
import Control.Monad

data Tree = Leaf Int | Node (Tree) Int (Tree)
          deriving (Show)

numbers :: String -> [[Int]]
numbers c = (map (map readInt) (map words (lines c)))

readInt = read :: String -> Int

createTree :: [[Int]] -> Tree
createTree ((n: _): []) = Leaf n
createTree ((n: _): xs) = Node (createTree (removeRightSide xs)) n (createTree (removeLeftSide xs))

foldTree :: Tree -> Tree
foldTree (Node (Leaf l1) n (Leaf l2)) = Leaf ((max l1 l2) + n)
foldTree (Node l n r) = foldTree (Node (foldTree l) n (foldTree r))

removeLeftSide :: [[Int]] -> [[Int]]
removeLeftSide [] = []
removeLeftSide ((_: []): xs) = removeLeftSide xs
removeLeftSide ((_: ns): xs) = ns : removeLeftSide xs

removeRightSide :: [[Int]] -> [[Int]]
removeRightSide [] = []
removeRightSide ((_: []): xs) = removeRightSide xs
removeRightSide (x: xs) = (init x) : (removeRightSide xs)

calc fname = do
  contents <- readFile fname
  let ns = numbers contents
  print $ show ns
  let t = foldTree (createTree ns)
  print $ show t