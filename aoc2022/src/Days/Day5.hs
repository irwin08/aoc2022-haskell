{-# Language LambdaCase #-}
module Days.Day5 () where

import Data.List.Split
import Data.Maybe
import qualified Data.Map as Map
import Data.Map (Map, (!))

type Warehouse = Map Int String


warehouse :: Warehouse
warehouse = Map.fromList [(1, "FGVRJLD"), (2, "SJHVBMPT"), (3, "CPGDFMHV"), (4, "QGNPDM"), (5, "FNHLJ"),
                          (6, "ZTGDQVFN"), (7, "LBDF"), (8, "NDVSBJM"), (9, "DLG")]


executeOperation :: Warehouse -> (Int, Int, Int) -> Warehouse
executeOperation warehouse (moveCount, from, to) =
  Map.insert from newFrom (Map.insert to newTo warehouse)
  where
    (toPush, newFrom) = splitAt moveCount (warehouse ! from)
    newTo = toPush ++ warehouse ! to -- add reverse to front for part 1 solution


extractMoves :: String -> (Int,Int,Int)
extractMoves line =
  let lst = splitOn " " line
  in (read (lst !! 1) :: Int, read (lst !! 3) :: Int, read (lst !! 5) :: Int)


main :: IO ()
main = do
  content <- readFile "data\\day5-p1.txt"
  let linesOfFile = lines content
      moveList = map extractMoves linesOfFile
      res = foldl executeOperation warehouse moveList
      output = foldl (++) "" $ take 1 <$> Map.elems res
  print output
  
