module Year2022.Day4 (countIntersectingSections, countPartiallyIntersectingSections, splitBy) where
--------------------
-- Task 1
--------------------
data Section = Section {start::Int, end::Int} deriving Show

countIntersectingSections :: IO ()
countIntersectingSections = do
  result <- length . filter (==True) . map (checkIntersection . map parseSection . splitBy ',') . lines <$> readFile "inputs/input4_1.txt"
  print result

splitBy :: Eq a => a -> [a] -> [[a]]
splitBy _ [] = []
splitBy del str = take delIndex str : splitBy del (drop (delIndex + 1) str)
  where
    delIndex = length . takeWhile (/=del) $ str

parseSection :: [Char] -> Section
parseSection section = Section s e
  where
    [strStart, strEnd] = splitBy '-' section
    s = read strStart :: Int
    e = read strEnd :: Int

checkIntersection :: [Section] -> Bool
checkIntersection [Section s1 e1, Section s2 e2] = result
  where
    result
      | s1 >= s2 && e1 <= e2 = True
      | s1 <= s2 && e1 >= e2 = True
      | otherwise = False

--------------------
-- Task 2
--------------------

checkPartialIntersection :: [Section] -> Bool
checkPartialIntersection [Section s1 e1, Section s2 e2] = result
  where
    result
      | s1 >= s2 && s1 <= e2 = True
      | s1 <= s2 && s2 <= e1 = True
      | otherwise = False

countPartiallyIntersectingSections :: IO ()
countPartiallyIntersectingSections = do
  result <- length . filter (==True) . map (checkPartialIntersection . map parseSection . splitBy ',') . lines <$> readFile "inputs/input4_1.txt"
  print result
