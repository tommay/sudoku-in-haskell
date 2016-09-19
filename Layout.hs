module Layout
(
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

import qualified Data.List as List

classic :: Int -> [Int]
classic n =
  List.nub [n, 80 - n]

reflectLeftRight :: Int -> Int
reflectLeftRight n =
  let col = n `mod` 9
  in n - col + 8 - col

leftRight :: Int -> [Int]
leftRight n =
  List.nub [n, reflectLeftRight n]

reflectUpDown :: Int -> Int
reflectUpDown n =
  let rowish = (n `div` 9) * 9
  in n - rowish + 72 - rowish

leftRightUpDown :: Int -> [Int]
leftRightUpDown n =
  let leftRightSets = leftRight n
  in List.nub $ leftRightSets ++ map reflectUpDown leftRightSets

identicalSquares :: Int -> [Int]
identicalSquares n =
  let col = n `mod` 3
      row = (n `div` 9) `mod` 3
      base = row*9 + col
  in List.nub $ map (base+) [0, 3, 6, 27, 30, 33, 54, 57, 60]

spinnySets :: [[Int]]
spinnySets = [
  [0, 8, 80, 72],
  [1, 17, 79, 63],
  [2, 26, 78, 54],
  [3, 35, 77, 45],
  [4, 44, 76, 36],
  [5, 53, 75, 27],
  [6, 62, 74, 18],
  [7, 71, 73, 9],
  [10, 16, 70, 64],
  [11, 25, 69, 55],
  [12, 34, 68, 46],
  [13, 43, 67, 37],
  [14, 52, 66, 28],
  [15, 61, 65, 19],
  [20, 24, 60, 56],
  [21, 33, 59, 47],
  [22, 42, 58, 38],
  [23, 51, 57, 29],
  [30, 32, 50, 48],
  [31, 41, 49, 39],
  [40]]

spinny :: Int -> [Int]
spinny n =
  case List.find (\set -> n `elem` set) spinnySets of
    Just set -> set

random :: Int -> [Int]
random n = [n]

reflectDiagonally :: Int -> Int
reflectDiagonally n =
  let (row, col) = rowcol n
  in col*9 + row

diagonal :: Int -> [Int]
diagonal n = 
  List.nub [n, reflectDiagonally n]

reflectDiagonally' :: Int -> Int
reflectDiagonally' n =
  let (row, col) = rowcol n
      col' = 8 - col
      row' = 8 - row
  in col'*9 + row'

diagonal' :: Int -> [Int]
diagonal' n = 
  List.nub [n, reflectDiagonally' n]

doubleDiagonal :: Int -> [Int]
doubleDiagonal n =
  let diagonalSets = diagonal n
  in List.nub $ diagonalSets ++ map reflectDiagonally' diagonalSets

wtf :: Int -> [Int]
wtf n = 
  let col = 8 - (n `mod` 9)
      row = n `div` 9
  in List.nub [n, col*9 + row]

wtf2 :: Int -> [Int]
wtf2 n = 
  let (row, col) = rowcol (80 - n)
  in List.nub [n, col*9 + row]

rowcol :: Int -> (Int, Int)
rowcol n =
  (n `div` 9, n `mod` 9)



