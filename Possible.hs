-- This all works!

module Possible
( Possible
, new
, remove
, size
, toList
) where

-- An object that maintains the possible digits for a Position.

data Possible = Possible [Int] deriving (Show)

new :: Possible
new = Possible [1..9]

-- XXX reverse the arg order?
remove :: Possible -> Int -> Possible
remove this digit = Possible $ filter (/= digit) $ toList this

-- With arg order reversed.
xremove :: Int -> Possible -> Possible
xremove n = Possible . filter (/= n) . toList

size :: Possible -> Int
size = length . toList

toList :: Possible -> [Int]
toList (Possible list) = list
