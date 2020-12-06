module Main where

import Data.Foldable (traverse_)
import Lib
import Utils

main :: IO ()
main = do
  let problems = [day1]
  traverse_ (>>= ppProblem) problems

day1 = do
  input <- listFromFile "data/day_1.txt"
  let result = reportRepair input
  pure $ Problem 1 "Report Repair" result

-- | Pretty print a problem
ppProblem :: Show a => Problem a -> IO ()
ppProblem problem = do
  putStrLn $ unlines [divider, show problem, divider]
  where
    divider = take 80 $ cycle "-"
