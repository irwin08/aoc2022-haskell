module Days.Day7 () where

import Debug.Trace
import Data.Tree
import Data.List
import Control.Monad

removeAtN :: Int -> [a] -> [a]
removeAtN _ [] = []
removeAtN 0 (x:xs) = xs
removeAtN n (x:xs) = x : removeAtN (n-1) xs
  
addNode :: Tree (String, String) -> (String, String) -> [Int] -> Tree (String, String)
addNode root addition [] = Node (rootLabel root) (subForest root ++ [Node addition []])
addNode root addition (x:xs) = trace ("x: " ++ (show x) ++ "subforest: " ++ (show $ subForest root)) Node (rootLabel root) $ removeAtN x (subForest root) ++ [addNode (subForest root !! x) addition xs]

processInstructions :: [String] -> [Int] -> Tree (String, String) -> Tree (String, String)
processInstructions [] _ accTree = accTree
processInstructions (instruction:instructions) accLocation accTree = case length instruction_components of
  3 -> (case (instruction_components !! 1) of
          "cd" -> (case (instruction_components !! 2) of
                     ".." -> processInstructions instructions (reverse $ tail $ reverse accLocation) accTree
                     _    -> processInstructions instructions (reverse $ (1 + maximum (if null accLocation then [-1] else accLocation)) : reverse accLocation) (addNode accTree ("Dir", instruction_components !! 2) accLocation))
          _    -> processInstructions instructions accLocation accTree) -- noop because we are listing the input that will be below, so we will get to it eventually
  2 -> (case (instruction_components !! 0) of
          "$"   -> processInstructions instructions accLocation accTree -- noop again. This is just ls
          "dir" -> processInstructions instructions accLocation accTree -- noop, handled above
          _     -> trace ("Trace instruction_components: " ++ (show instruction_components) ++ (show accLocation)) processInstructions instructions accLocation (addNode accTree (instruction_components !! 0, instruction_components !! 1) accLocation))
  _ -> accTree -- this shouldn't happen
  where instruction_components = words instruction

main :: IO ()
main = do
  content <- readFile "data\\day7-p1.txt"
  let instructions = lines content
  let processedInstructions = processInstructions instructions [] $ Node ("ROOT", "ROOT") []
  let simplifiedTree = fmap fst processedInstructions
  putStrLn $ drawTree simplifiedTree
  let sizes =  filter (\x -> x `notElem` ["ROOT", "Dir"]) $ flatten simplifiedTree
  putStrLn $ show $ sum $ map(read :: String -> Int) sizes
  
  
  

