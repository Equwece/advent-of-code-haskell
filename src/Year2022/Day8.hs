module Year2022.Day8 where

import Data.Char (digitToInt)
import Data.Matrix (Matrix, fromLists, getCol, getElem, getRow)
import Data.Vector (slice, toList)

--------------------
-- Task 1
--------------------

countVisibleTrees :: IO ()
countVisibleTrees = do
  rawTreeData <- map (foldl (\acc x -> acc ++ [digitToInt x]) []) . lines <$> readFile "inputs/input8.txt"
  let treeMatrix = fromLists rawTreeData
  print $ processMatrixRows treeMatrix 1

processMatrixRows :: (Ord a1, Num a2) => Matrix a1 -> Int -> a2
processMatrixRows treeMatrix row = result
  where
    resultRow = processMatrixColumns treeMatrix row 1
    result
      | row == length (getCol 0 treeMatrix) = resultRow
      | otherwise = resultRow + processMatrixRows treeMatrix (row + 1)

processMatrixColumns :: (Ord a1, Num a2) => Matrix a1 -> Int -> Int -> a2
processMatrixColumns treeMatrix row column = result
  where
    currentElem = getElem row column treeMatrix
    elemRow = getRow row treeMatrix
    elemCol = getCol column treeMatrix
    leftCheck acc nextElem = if nextElem >= currentElem then acc && False else acc && True
    rightCheck nextElem acc = if nextElem >= currentElem then acc && False else acc && True
    leftVisibility = foldl leftCheck True (slice 0 (column - 1) elemRow)
    rightVisibility = foldr rightCheck True (slice column (length elemRow - column) elemRow)
    topVisibility = foldl leftCheck True (slice 0 (row - 1) elemCol)
    bottomVisibility = foldr rightCheck True (slice row (length elemCol - row) elemCol)
    resultVisibility = if leftVisibility || rightVisibility || topVisibility || bottomVisibility then 1 else 0
    result
      | column == length (getCol 0 treeMatrix) = resultVisibility
      | otherwise = resultVisibility + processMatrixColumns treeMatrix row (column + 1)

--------------------
-- Task 2
--------------------

processMatrixRows2 :: Ord a => Matrix a -> Int -> Int -> Int
processMatrixRows2 treeMatrix row maxResult = result
  where
    resultRow = processMatrixColumns2 treeMatrix row 1 maxResult
    newMaxResult
      | resultRow > maxResult = resultRow
      | otherwise = maxResult
    result
      | row == length (getCol 0 treeMatrix) = newMaxResult
      | otherwise = processMatrixRows2 treeMatrix (row + 1) newMaxResult

processMatrixColumns2 :: Ord a => Matrix a -> Int -> Int -> Int -> Int
processMatrixColumns2 treeMatrix row column maxResult = result
  where
    currentElem = getElem row column treeMatrix
    elemRow = getRow row treeMatrix
    elemCol = getCol column treeMatrix
    checkVisibility [] = []
    checkVisibility (newElem : reminder)
      | newElem < currentElem = newElem : checkVisibility reminder
      | otherwise = [newElem]
    leftVisibility = length . checkVisibility . reverse . toList $ slice 0 (column - 1) elemRow
    rightVisibility = length . checkVisibility . toList $ slice column (length elemRow - column) elemRow
    topVisibility = length . checkVisibility . reverse . toList $ slice 0 (row - 1) elemCol
    bottomVisibility = length . checkVisibility . toList $ slice row (length elemCol - row) elemCol
    resultVisibility = leftVisibility * rightVisibility * topVisibility * bottomVisibility
    newMaxResult
      | resultVisibility > maxResult = resultVisibility
      | otherwise = maxResult
    result
      | column == length (getCol 0 treeMatrix) = newMaxResult
      | otherwise = processMatrixColumns2 treeMatrix row (column + 1) newMaxResult

countVisibleTrees2 :: IO ()
countVisibleTrees2 = do
  rawTreeData <- map (foldl (\acc x -> acc ++ [digitToInt x]) []) . lines <$> readFile "inputs/input8.txt"
  let treeMatrix = fromLists rawTreeData
  print $ processMatrixRows2 treeMatrix 1 0
