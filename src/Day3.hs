module Day3 (getWrongItemPropertySum, getGroupItemSum) where
import Data.Char (ord, isUpper)

--------------------
-- Task 1
--------------------

getWrongItemPropertySum :: IO ()
getWrongItemPropertySum = do
  result <- sum . map (getItemPriority . getWrongItemType . (\s -> splitAt (length s `div` 2) s)) . lines <$> readFile "inputs/input3_1.txt"
  print result

getWrongItemType :: ([Char], [Char]) -> Char
getWrongItemType (firstPart, secondPart) = result
  where
    iter = [(el, secondPart) | el <- firstPart]
    foldIter (x:xs) = res
      where
        res
          | uncurry elem x = fst x
          | otherwise = foldIter xs
    foldIter [] = ' '
    result = foldIter iter

getItemPriority :: Char -> Int
getItemPriority item = result
  where
    result
      | isUpper item = ord item - 38
      | otherwise = ord item - 96

--------------------
-- Task 2
--------------------
getGroupItemSum :: IO ()
getGroupItemSum  = do
  result <- sum . map (getItemPriority . getCommonItemType) . splitToGroups . lines <$> readFile "inputs/input3_1.txt"
  print result

splitToGroups :: [a] -> [[a]]
splitToGroups [] = []
splitToGroups inventory = take 3 inventory : splitToGroups (drop 3 inventory)

getCommonItemType :: [String] -> Char
getCommonItemType [x, y, z] = result
  where
    result = foldl (\a c -> if elem c y && elem c z then c else a) (head x) x
getCommonItemType _ = ' '
