module Main where

import Data.Foldable (traverse_)
import Lib
import Utils

main :: IO ()
main = do
  let problems =
        [ day1part1,
          day1part2
        ]
  traverse_ (>>= ppProblem) problems

day1part1 = do
  input <- listFromFile "data/day_1.txt"
  let result = reportRepair input
  pure $ Problem 1 "Report Repair: Part 1" result

day1part2 = do
  -- input is the same as day 1
  input <- listFromFile "data/day_1.txt"
  let result = reportRepairPart2 input
  pure $ Problem 1 "Report Repair: Part 2" result

-- | Pretty print a problem
ppProblem :: Show a => Problem a -> IO ()
ppProblem problem = do
  putStrLn $ unlines [divider, show problem, divider]
  where
    divider = take 80 $ cycle "-"
