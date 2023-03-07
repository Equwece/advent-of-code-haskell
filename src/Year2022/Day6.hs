module Year2022.Day6 where

--------------------
-- Task 1
--------------------

processPacket :: IO ()
processPacket = do
  result <- readFile "inputs/input6.txt"
  let markerPosition = findMarker result [] 1 packetSize
      packetSize = 4
  print markerPosition

findMarker :: [Char] -> [Char] -> Int -> Int -> Int
findMarker [] _ counter _ = counter
findMarker (newChar : reminder) currentSequence counter packetSize = result
  where
    newSequence = if length currentSequence < packetSize then currentSequence ++ [newChar] else drop 1 currentSequence ++ [newChar]
    result
      | isMarker newSequence packetSize = counter
      | otherwise = findMarker reminder newSequence (counter + 1) packetSize

isMarker :: Eq a => [a] -> Int -> Bool
isMarker sq packetSize = result
  where
    result
      | length sq /= packetSize = False
      | otherwise = checkUniqueness sq 0

    checkUniqueness currentSequence counter = checkResult
      where
        uniqueness = (currentSequence !! counter) `notElem` (take counter currentSequence ++ drop (counter + 1) currentSequence)
        nextIter
          | counter + 1 == packetSize = True
          | otherwise = checkUniqueness currentSequence (counter + 1)
        checkResult = if not uniqueness then False else nextIter

--------------------
-- Task 2
--------------------

processPacket2 :: IO ()
processPacket2 = do
  result <- readFile "inputs/input6.txt"
  let markerPosition = findMarker result [] 1 packetSize
      packetSize = 14
  print markerPosition
