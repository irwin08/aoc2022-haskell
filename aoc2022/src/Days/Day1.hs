module Days.Day1 () where

import Data.List.Split
import Data.List

type ItemCalories = Int


-- each elf holds a collection of items with calories

totalOfFattest :: [[ItemCalories]] -> ItemCalories
totalOfFattest elves = foldl (\x y -> if x >= y then x else y) 0 $ fmap (foldl (+) 0) elves

totalOfTop3Fattest :: [[ItemCalories]] -> ItemCalories
totalOfTop3Fattest elves = foldl (+) 0 $ take 3 $ sortBy (flip compare) $ fmap (foldl (+) 0) elves

spliterino :: [String] -> [[String]]
spliterino strList =  splitWhen (\x -> x == "") strList

main :: IO ()
main = do
  content <- readFile "data\\day1-p1.txt"  
  let linesOfFile = lines content
  let formatted :: [[ItemCalories]] = (map . map) (read :: String -> ItemCalories) $ spliterino linesOfFile

  -- part 1
  
  print $ totalOfFattest formatted
  
  -- part 2

  print $ totalOfTop3Fattest formatted


