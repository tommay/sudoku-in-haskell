module Util (
  slices,
) where

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
