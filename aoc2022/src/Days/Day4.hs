module Days.Day4 () where

import Data.List.Split



containsSubset :: Eq a => [a] -> [a] -> Bool
containsSubset firstList secondList =
  (all (\x -> x `elem` secondList) firstList) || (all (\x -> x `elem` firstList) secondList)

containsOverlap :: Eq a => [a] -> [a] -> Bool
containsOverlap firstList secondList =
  any (\x -> x `elem` secondList) firstList


main :: IO ()
main = do
  content <- readFile "data\\day4-1.txt"
  let linesOfFile = lines content
  -- Part 1
  print $ length $ filter (==True) $ map (\y -> containsSubset (y !! 0) (y !! 1)) $ (map . map) (\x -> [((read $ ((splitOn "-" x) !! 0)) :: Int)..(read $ ((splitOn "-" x) !! 1) :: Int)]) (map (splitOn ",") linesOfFile)
  -- Part 2
  print $ length $ filter (==True) $ map (\y -> containsOverlap (y !! 0) (y !! 1)) $ (map . map) (\x -> [((read $ ((splitOn "-" x) !! 0)) :: Int)..(read $ ((splitOn "-" x) !! 1) :: Int)]) (map (splitOn ",") linesOfFile)
