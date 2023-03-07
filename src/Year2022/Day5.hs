module Year2022.Day5 (supplyStacks1, supplyStacks2) where

import Control.Lens
import Year2022.Day4 (splitBy)

--------------------
-- Task 1
--------------------

newtype Stack = Stack {crates :: [Char]} deriving (Show)
data Command = Command {amount :: Int, from :: Int, to :: Int} deriving (Show)

supplyStacks1 :: IO ()
supplyStacks1 = moveCrates runCommands

moveCrates :: Foldable t => ([Stack] -> [Command] -> t Stack) -> IO ()
moveCrates runCommandsFunc = do
  linedData <- lines <$> readFile "inputs/input5_1.txt"
  let
    initialRawData = map (splitBy ' ') . takeWhile (/= "") $ linedData
    cratesData = drop 1 . reverseList $ initialRawData
    compressedStackList = map compressStackList cratesData
    stacksLen = length . head $ compressedStackList
    stacks = [Stack [] | _ <- [1 .. stacksLen]]
    filledStacks = fillStackList stacks compressedStackList
    commands = map (parseCommand . splitBy ' ') . drop 1 . dropWhile (/= "") $ linedData
    resultStacks = runCommandsFunc filledStacks commands
    result = foldl (\acc (Stack stackCrates) -> acc ++ [head stackCrates]) [] resultStacks
  print result

reverseList :: [a] -> [a]
reverseList = foldl (\acc x -> x : acc) []

compressStackList :: [String] -> [String]
compressStackList cratesLine = result
 where
  compressCratesLine (crate : line) = case crate of
    "" -> crate : compressCratesLine (drop 1 line)
    _ -> crate : compressCratesLine line
  compressCratesLine [] = []
  compressedLine = compressCratesLine . compressCratesLine $ cratesLine
  result = compressedLine

fillStackList :: [Stack] -> [[[Char]]] -> [Stack]
fillStackList stacks [] = stacks
fillStackList stacks (crateLine : reminder) = fillStackList newStacks reminder
 where
  addCrate acc (['[', crateChar, ']'], Stack stackCrates) = acc ++ [Stack $ crateChar : stackCrates]
  addCrate acc (_, stack) = acc ++ [stack]
  newStacks = foldl addCrate [] (zip crateLine stacks)

parseCommand :: [String] -> Command
parseCommand rawCommand = Command cratesAmount fromStack toStack
 where
  cratesAmount = read $ rawCommand !! 1 :: Int
  fromStack = read $ rawCommand !! 3 :: Int
  toStack = read $ rawCommand !! 5 :: Int

runCommands :: [Stack] -> [Command] -> [Stack]
runCommands stacks [] = stacks
runCommands stacks ((Command cratesAmount fromStackNum toStackNum) : reminder) = runCommands newStacks reminder
 where
  (Stack fromStackCrates) = stacks !! (fromStackNum - 1)
  commandToStack = stacks !! (toStackNum - 1)
  commandCrates = take cratesAmount fromStackCrates
  newToStack = foldl (\(Stack stackCrates) crate -> Stack (crate : stackCrates)) commandToStack commandCrates
  newFromStack = Stack (drop cratesAmount fromStackCrates)
  newStacks = stacks & ix (fromStackNum - 1) .~ newFromStack & ix (toStackNum - 1) .~ newToStack

--------------------
-- Task 2
--------------------

supplyStacks2 :: IO ()
supplyStacks2 = moveCrates runCommandsV2

runCommandsV2 :: [Stack] -> [Command] -> [Stack]
runCommandsV2 stacks [] = stacks
runCommandsV2 stacks ((Command cratesAmount fromStackNum toStackNum) : reminder) = runCommandsV2 newStacks reminder
 where
  (Stack fromStackCrates) = stacks !! (fromStackNum - 1)
  (Stack toStackCrates) = stacks !! (toStackNum - 1)
  commandCrates = take cratesAmount fromStackCrates
  newToStack = Stack (commandCrates ++ toStackCrates)
  newFromStack = Stack (drop cratesAmount fromStackCrates)
  newStacks = stacks & ix (fromStackNum - 1) .~ newFromStack & ix (toStackNum - 1) .~ newToStack
