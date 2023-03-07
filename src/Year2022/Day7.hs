module Year2022.Day7 where

import Control.Lens
import Data.List (elemIndex, sort)
import Year2022.Day1 (readStringToInt)

--------------------
-- Task 1
--------------------

data Node = File {size :: Int, name :: String} | Folder {content :: [Node], name :: String} deriving (Show, Eq)

getDirSize :: IO ()
getDirSize = do
  result <- map words . lines <$> readFile "inputs/input7.txt"
  let fileTree = Folder {content = [Folder {content = [], name = "/"}], name = ""}
      newFileTree = runCommands result fileTree ["/"]
      nodeList = filter (\(nodeSize, _) -> nodeSize < 100000) . getSizesOfRequiredDirs $ newFileTree
  print . sum $ foldl (\acc (nodeSize, _) -> acc ++ [nodeSize]) [] nodeList

insertNodeList :: [String] -> Node -> [Node] -> Node
insertNodeList _ fileTree [] = fileTree
insertNodeList path fileTree (node : reminder) = insertNodeList path newFileTree reminder
  where
    newFileTree = insertNode path fileTree node

insertNode :: [String] -> Node -> Node -> Node
insertNode [] node@(Folder folderContent currentFolderName) newNode
  | newNode `notElem` folderContent = Folder (newNode : folderContent) currentFolderName
  | otherwise = node
insertNode (name : pathReminder) (Folder folderContent currentFolderName) newNode = newFileTree
  where
    checkNodeName requiredName (Folder _ folderName)
      | folderName == requiredName = True
      | otherwise = False
    checkNodeName _ (File _ _) = False

    nextNode = head $ filter (checkNodeName name) folderContent
    (Just nextNodeIndex) = elemIndex nextNode folderContent
    newFileTree = Folder (folderContent & ix nextNodeIndex .~ insertNode pathReminder nextNode newNode) currentFolderName

runCommands :: [[String]] -> Node -> [String] -> Node
runCommands [] fileTree _ = fileTree
runCommands (command : reminder) fileTree path = runCommands reminder newFileTree newPath
  where
    (newFileTree, newPath) = case command of
      ["$", "cd", cdPath] -> runChangeDirCommand cdPath fileTree path
      ["$", "ls"] -> runListCommand fileTree path reminder
      _ -> (fileTree, path)

runChangeDirCommand :: String -> Node -> [String] -> (Node, [String])
runChangeDirCommand cdPath fileTree currentPath
  | cdPath == "/" = (fileTree, [cdPath])
  | cdPath == ".." = (fileTree, take (length currentPath - 1) currentPath)
  | otherwise = (fileTree, currentPath ++ [cdPath])

runListCommand :: Node -> [String] -> [[String]] -> (Node, [String])
runListCommand fileTree currentPath commands = result
  where
    dirList = takeWhile (\x -> head x /= "$") commands
    parseDirList [] = []
    parseDirList (entry : listReminder) = parsedElem : parseDirList listReminder
      where
        parsedElem = case entry of
          ["dir", nodeName] -> Folder [] nodeName
          [nodeSize, nodeName] -> File (readStringToInt nodeSize) nodeName
    parsedDirList = parseDirList dirList
    result = (insertNodeList currentPath fileTree parsedDirList, currentPath)

countNodeSize :: Node -> Int
countNodeSize (File fileSize _) = fileSize
countNodeSize (Folder folderContent _) = folderSize
  where
    folderSize = foldl (\acc node -> acc + countNodeSize node) 0 folderContent

getSizesOfRequiredDirs :: Node -> [(Int, String)]
getSizesOfRequiredDirs (File _ _) = []
getSizesOfRequiredDirs node@(Folder folderContent folderName) = (folderSize, folderName) : reminderDirSizes
  where
    folderSize = countNodeSize node
    reminderDirSizes = foldl (\acc nextNode -> acc ++ getSizesOfRequiredDirs nextNode) [] folderContent

--------------------
-- Task 2
--------------------

getDirSize2 :: IO ()
getDirSize2 = do
  result <- map words . lines <$> readFile "inputs/input7.txt"
  let fileTree = Folder {content = [Folder {content = [], name = "/"}], name = ""}
      newFileTree = runCommands result fileTree ["/"]
      rootSize = countNodeSize newFileTree
      requiredSize = 30000000 - (70000000 - rootSize)
      (resultSize, _) = minimum . filter (\(nodeSize, _) -> nodeSize >= requiredSize) . getSizesOfRequiredDirs $ newFileTree
  print resultSize
