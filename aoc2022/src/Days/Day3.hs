module Days.Day3() where

import Data.Char
import Data.Maybe
import Data.Tuple
import Data.Tuple.Extra
import Data.List.Split
import Data.List (sortBy, groupBy, intersect)
import Data.Function (on)

ruckSack :: String
ruckSack = "vJrwpWtwJgWrhcsFMMfFFhFp"


-- We are assuming the compartments are evenly distributed here (hence the / 2)
divideRuckSack :: [Char] -> [[Char]]
divideRuckSack ruck = [take compartmentSize ruck, reverse (take compartmentSize (reverse ruck))]
  where
    ruckSize = length ruck
    compartmentSize = ruckSize `div` 2

 
-- this abstraction came out in part 2, was way worse before
findDuplicates :: Eq a => [[a]] -> [a]
findDuplicates listOfStrings = foldl intersect (head listOfStrings) listOfStrings

priority :: Char -> Int
priority c = snd $ head $ filter (\x -> (fst x :: Char) == c) priorityList
  where
    lowerCase = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z']
    upperCase = map (\x -> toUpper x) lowerCase
    priorityList = (zip lowerCase [1..]) ++ (zip upperCase [27..])
       
main :: IO ()
main = do
  content <- readFile "data\\day3-p1.txt"  
  let linesOfFile = lines content

  -- part 1
  print $ sum $ map (priority . head . findDuplicates . divideRuckSack) linesOfFile

  -- part 2
  print $ sum $ map priority (map (head . findDuplicates) (chunksOf 3 linesOfFile))



