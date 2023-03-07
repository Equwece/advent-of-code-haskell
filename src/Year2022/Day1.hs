module Year2022.Day1 (
  readStringToInt,
  countCalories,
  splitByNewLine,
  countTopThreeCalories,
)
where

import Data.List (sort)

countCalories :: IO ()
countCalories = do
  parsedResult <- maximum . map (sum . map readStringToInt) . splitByNewLine . lines <$> getContents
  print parsedResult

readStringToInt :: String -> Int
readStringToInt str = read str :: Int

splitByNewLine :: (Foldable t) => [t a] -> [[t a]]
splitByNewLine [] = []
splitByNewLine lns = if not . null $ newPart then newPart : splitByNewLine reminder else splitByNewLine reminder
 where
  getNewLineIndex [] i = i
  getNewLineIndex (x : xs) i
    | null x = i
    | otherwise = getNewLineIndex xs (i + 1)
  newLineIndex = getNewLineIndex lns 0
  newPart = take newLineIndex lns
  reminder = drop (newLineIndex + 1) lns

----------------------------------------
-- Task 2
----------------------------------------

countTopThreeCalories :: IO ()
countTopThreeCalories = do
  parsedResult <- sum . take 3 . reverse . sort . map (sum . map readStringToInt) . splitByNewLine . lines <$> getContents
  print parsedResult
