module Layout
(
  layout,
  classic,
  leftRight,
  leftRightUpDown,
  identicalSquares,
  spinny,
  random,
  diagonal,
  doubleDiagonal,
  wtf,
  wtf2,
) where

import qualified Data.Map as Map

classic :: Int -> [Int]
classic n =
  [n, 80 - n]

reflectLeftRight :: Int -> Int
reflectLeftRight n =
  let col = n `mod` 9
  in n - col + 8 - col

leftRight :: Int -> [Int]
leftRight n =
  [n, reflectLeftRight n]

reflectUpDown :: Int -> Int
reflectUpDown n =
  let rowish = (n `div` 9) * 9
  in n - rowish + 72 - rowish

leftRightUpDown :: Int -> [Int]
leftRightUpDown n =
  let leftRightSets = leftRight n
  in leftRightSets ++ map reflectUpDown leftRightSets

identicalSquares :: Int -> [Int]
identicalSquares n =
  let col = n `mod` 3
      row = (n `div` 9) `mod` 3
      base = row*9 + col
  in map (base +) [0, 3, 6, 27, 30, 33, 54, 57, 60]

spinny :: Int -> [Int]
spinny n =
  spinny' n []

spinny' :: Int -> [Int] -> [Int]
spinny' n result =
  if n `elem` result
    then result
    else
      let (row, col) = rowcol(n)
          row' = col
          col' = 8 - row
      in spinny' ((row' * 9) + col') (n : result)

random :: Int -> [Int]
random n = [n]

reflectDiagonally :: Int -> Int
reflectDiagonally n =
  let (row, col) = rowcol n
  in col*9 + row

diagonal :: Int -> [Int]
diagonal n = 
  [n, reflectDiagonally n]

reflectDiagonally' :: Int -> Int
reflectDiagonally' n =
  let (row, col) = rowcol n
      col' = 8 - col
      row' = 8 - row
  in col'*9 + row'

doubleDiagonal :: Int -> [Int]
doubleDiagonal n =
  let diagonalSets = diagonal n
  in diagonalSets ++ map reflectDiagonally' diagonalSets

wtf :: Int -> [Int]
wtf n = 
  let col = 8 - (n `mod` 9)
      row = n `div` 9
  in [n, col*9 + row]

wtf2 :: Int -> [Int]
wtf2 n = 
  let (row, col) = rowcol (80 - n)
  in [n, col*9 + row]

rowcol :: Int -> (Int, Int)
rowcol n =
  (n `div` 9, n `mod` 9)

layout :: (Int -> [Int]) -> [[Int]]
layout func =
  uniqBy minimum $ map (uniq . func) [0..80]

uniq :: Ord a => [a] -> [a]
uniq = uniqBy id

uniqBy :: Ord b => (a -> b) -> [a] -> [a]
uniqBy func list =
  Map.elems $ Map.fromList [(func e, e) | e <- list]
