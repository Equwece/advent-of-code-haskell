module Day4 (countIntersectingSections) where
import Data.List
--------------------
-- Task 1
--------------------
countIntersectingSections :: IO ()
countIntersectingSections = do
  result <- lines <$> readFile "inputs/input4_1.txt"
  -- splitOn
  print result
