module Util (
  Util.shuffle,
  Util.slices,
) where

import qualified Data.List as List
import System.Random as Random
import System.Random.Shuffle as Shuffle

shuffle :: Random.StdGen -> [a] -> [a]
-- shuffle' infinite loops on [], grr.
shuffle _ [] = []
shuffle rnd list =
  Shuffle.shuffle' list (length list) rnd

myShuffle :: Random.StdGen -> [a] -> [a]
myShuffle _ [] = []
myShuffle rnd list =
  let len = length list
      (n, newRnd) = Random.randomR (0, len - 1) rnd
      (first, (e : rest)) = List.splitAt n list
  in e : (myShuffle newRnd $ first ++ rest)

-- Slice an array up into sub-arrays of n elements, and return the
-- sub-arrays in a list.
--
-- You'd think there would be a function to do this but I can't find
-- one easily.  It will be good practice to roll my own.  This can be
-- done all kinds of ways, e.g., with a fold, but here I don't use
-- anything fancy.  Except splitAt is fancy.
--
slices :: Int -> [a] -> [[a]]
slices _ [] = []
slices n list =
  let (slice, rest) = splitAt n list
  in slice : slices n rest
