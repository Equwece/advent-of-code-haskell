module Day2 (getTotalScore, getTotalScore') where

--------------------
-- Task 1
--------------------

getTotalScore :: IO ()
getTotalScore = do
  result <- resolveMaybeResult 0 . map (calculateRoundScore . words) . lines <$> readFile "inputs/input2_1.txt"
  print (case result of
      Just value -> show value
      _ -> "Error")

resolveMaybeResult :: Int -> [Maybe Int] -> Maybe Int
resolveMaybeResult acc (x:xs) =
  case x of
    Just value -> resolveMaybeResult (acc + value) xs
    _ -> Nothing
resolveMaybeResult acc [] = Just acc

calculateRoundScore :: [String] -> Maybe Int
calculateRoundScore [x, y] = result
  where
    roundOutcomeScore = getRoundOutcomeScore (x, y)
    shapeScore = getShapeScore y
    result = (+) <$> roundOutcomeScore <*> shapeScore
calculateRoundScore _ = Nothing

getShapeScore :: String -> Maybe Int
getShapeScore shape =
  case shape of
    "X" -> Just 1
    "Y" -> Just 2
    "Z" -> Just 3
    _ -> Nothing
    
getRoundOutcomeScore :: (String, String) -> Maybe Int
getRoundOutcomeScore (comp, "X") = 
  case comp of
    "A" -> Just 3
    "B" -> Just 0
    "C" -> Just 6
    _ -> Nothing
getRoundOutcomeScore (comp, "Y") = 
  case comp of
    "A" -> Just 6
    "B" -> Just 3
    "C" -> Just 0
    _ -> Nothing
getRoundOutcomeScore (comp, "Z") = 
  case comp of
    "A" -> Just 0
    "B" -> Just 6
    "C" -> Just 3
    _ -> Nothing
getRoundOutcomeScore (_, _) = Nothing

--------------------
-- Task 2
--------------------

getTotalScore' :: IO ()
getTotalScore'= do
  result <- resolveMaybeResult 0 . map (calculateRoundScore' . words) . lines <$> readFile "inputs/input2_1.txt"
  print (case result of
      Just value -> show value
      _ -> "Error")

calculateRoundScore' :: [String] -> Maybe Int
calculateRoundScore' [x, y] = result
  where
    roundOutcomeScore = getRoundOutcomeScore' (x, y)
    result = roundOutcomeScore
calculateRoundScore' _ = Nothing

getRoundOutcomeScore' :: (String, String) -> Maybe Int
getRoundOutcomeScore' (comp, "X") = 
  case comp of
    "A" -> Just 3
    "B" -> Just 1
    "C" -> Just 2
    _ -> Nothing
getRoundOutcomeScore' (comp, "Y") = 
  case comp of
    "A" -> Just 4
    "B" -> Just 5
    "C" -> Just 6
    _ -> Nothing
getRoundOutcomeScore' (comp, "Z") = 
  case comp of
    "A" -> Just 8
    "B" -> Just 9
    "C" -> Just 7
    _ -> Nothing
getRoundOutcomeScore' (_, _) = Nothing
