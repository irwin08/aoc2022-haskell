module Days.Day6 () where

import Data.List
import Control.Applicative

slidingWindow :: Int -> String -> [String]
slidingWindow n text =
  (transpose'. take n . tails) text

transpose' :: [[a]] -> [[a]]
transpose' = getZipList . sequenceA . map ZipList

unique :: Eq a => [a] -> Bool
unique [] = True
unique (x:xs) = x `notElem` xs && unique xs

numberOfUnique :: Int
numberOfUnique = 14

main :: IO ()
main = do
  content <- readFile "data\\day6-p1.txt"
  putStrLn $ show $ fmap (\x -> x + numberOfUnique) $ findIndex (\x -> x) $ map (\xs -> unique xs) $ slidingWindow numberOfUnique content
