module Lib
  ( reportRepair,
    reportRepairPart2,
  )
where

-- | Get all subsequences of a list of size N
--
-- We construct each subsequence by finding all of the subsequences of size `n`
-- that contain x, and appending that to the subsequences of size `n` that
-- don't contain x, repeating this recursively, and filtering out any
-- subsequences that are less than the requested size.
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
reportRepair xs = product answer
  where
    allPairs = subsequences 2 xs
    answer = head $ filter ((== 2020) . sum) allPairs

-- | Day 2
--
-- Find three entries in the list that sum to 2020 and then multiply them
-- together
reportRepairPart2 :: [Int] -> Int
reportRepairPart2 xs = product answer
  where
    allPairs = subsequences 3 xs
    answer = head $ filter ((== 2020) . sum) allPairs
