module Layout
(
  getLayout,
  getLayoutStrings,
) where

import qualified Data.Char as Char
import qualified Data.Map as Map

layouts =
  [
    ("classic", classic),
    ("leftRight", leftRight),
    ("leftRightUpDown", leftRightUpDown),
    ("identicalSquares", identicalSquares),
    ("spinny", spinny),
    ("random", random),
    ("diagonal", diagonal),
    ("doubleDiagonal", doubleDiagonal),
    ("wtf", wtf),
    ("wtf2", wtf2)
  ]

classic :: Int -> [Int]
classic n =
  [n, 80 - n]

reflectLeftRight :: Int -> Int
reflectLeftRight n =
  let (_, col) = rowcol n
  in n - col + 8 - col

leftRight :: Int -> [Int]
leftRight n =
  [n, reflectLeftRight n]

reflectUpDown :: Int -> Int
reflectUpDown n =
  let (row, _) = rowcol n
      rowish = row * 9
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
      row' = col
      col' = row
  in row' * 9 + col'

diagonal :: Int -> [Int]
diagonal n = 
  [n, reflectDiagonally n]

reflectDiagonally' :: Int -> Int
reflectDiagonally' n =
  let (row, col) = rowcol n
      row' = 8 - col
      col' = 8 - row
  in row' * 9 + col'

doubleDiagonal :: Int -> [Int]
doubleDiagonal n =
  let diagonalSets = diagonal n
  in diagonalSets ++ map reflectDiagonally' diagonalSets

wtf :: Int -> [Int]
wtf n = 
  let (row, col) = rowcol n
      row' = 8 - col
      col' = row
  in [n, row' * 9 + col']

wtf2 :: Int -> [Int]
wtf2 n = 
  let (row, col) = rowcol (80 - n)
      row' = col
      col' = row
  in [n, row' * 9 + col']

toLower :: String -> String
toLower =
  map Char.toLower

getLayout :: String -> Maybe [[Int]]
getLayout style =
  let layoutMap = Map.fromList $ [(toLower name, func) | (name, func) <- layouts]
      maybeFunc = Map.lookup (toLower style) layoutMap
  in case maybeFunc of
       Nothing -> Nothing
       Just func -> Just $ uniqBy minimum $ map (uniq . func) [0..80]

getLayoutStrings :: [String]
getLayoutStrings =
  map fst layouts

rowcol :: Int -> (Int, Int)
rowcol n =
  (n `div` 9, n `mod` 9)

uniq :: Ord a => [a] -> [a]
uniq = uniqBy id

uniqBy :: Ord b => (a -> b) -> [a] -> [a]
uniqBy func list =
  Map.elems $ Map.fromList [(func e, e) | e <- list]
