module Lib where

-- | Get all subsequences of a list of size N
subsequences :: Int -> [a] -> [[a]]
subsequences 0 _ = [[]]
subsequences _ [] = [[]]
subsequences n xs@(x : rest) =
  if n > length xs
    then [[]]
    else filterNulls (withX ++ withoutX)
  where
    withX = (:) x <$> subsequences (n - 1) rest
    withoutX = subsequences n rest
    filterNulls = filter (not . null)

-- | Day 1
--
-- Find two entries in the list that sum to 2020 and then multiply them
-- together
reportRepair :: [Int] -> Int
reportRepair xs = multiply answer
  where
    allPairs = subsequences 2 xs
    answer = head $ filter (\(x : y : _) -> x + y == 2020) allPairs
    multiply (x : y : _) = x * y
