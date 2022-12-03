module Days.Day2 () where




-- One interseting thing to do here woul be to declare one datatype Move and then have both choices be aliases
-- I'm not sure how to do this quite yet, so I may come back and modify this in the future.
--                     R | P | S
data Choice =  R | P | S deriving (Eq, Show)
instance Ord Choice where
  compare R R = EQ
  compare P P = EQ
  compare S S = EQ
  
  compare R P = LT
  compare P R = GT
  compare R S = GT
  compare S R = LT
  compare P S = LT
  compare S P = GT

data Result = W | L | D deriving (Eq, Show)

getWinAgainst :: Choice -> Choice
getWinAgainst choice
  | choice == R = P
  | choice == P = S
  | choice == S = R

getLossAgainst :: Choice -> Choice
getLossAgainst choice
  | choice == R = S
  | choice == P = R
  | choice == S = P

getDrawAgainst :: Choice -> Choice
getDrawAgainst choice
  | choice == R = R
  | choice == P = P
  | choice == S = S



getResultAgainst :: Choice -> Result -> Choice
getResultAgainst choice result
  | result == W = getWinAgainst choice
  | result == L = getLossAgainst choice
  | result == D = getDrawAgainst choice

-- instance Show Choice where
--   show c
--     | c == R = show "R"
--     | c == P = show "P"
--     | c == S = show "S"
  

valueLookup :: Choice -> Int
valueLookup R = 1
valueLookup P = 2
valueLookup S = 3

-- Opponent Choice -> Player Choice -> Score
roundScore :: Choice -> Choice -> Int
roundScore op pl
  | op > pl = valueLookup pl
  | op == pl = valueLookup pl + 3
  | otherwise = valueLookup pl + 6

moveListSample :: [[String]]
moveListSample = [["A", "Y"], ["B", "X"], ["C", "Z"]]

convertStrToPlayerChoice :: String -> Choice
convertStrToPlayerChoice "X" = R
convertStrToPlayerChoice "Y" = P
convertStrToPlayerChoice "Z" = S

convertStrToOpponentChoice :: String -> Choice
convertStrToOpponentChoice "A" = R
convertStrToOpponentChoice "B" = P
convertStrToOpponentChoice "C" = S

convertStrToResult :: String -> Result
convertStrToResult "X" = L
convertStrToResult "Y" = D
convertStrToResult "Z" = W

main :: IO ()
main = do
  content <- readFile "data\\day2-p1.txt"  
  let linesOfFile = lines content

  -- Part 1
  print $ foldl (+) 0 $ map (\x -> (roundScore (convertStrToOpponentChoice $ x !! 0) (convertStrToPlayerChoice $ x !! 1))) $ map words linesOfFile

  -- Part 2
  print $ foldl (+) 0 $ map (\x -> (roundScore (convertStrToOpponentChoice $ x !! 0) (getResultAgainst (convertStrToOpponentChoice $ x !! 0) (convertStrToResult $ x !! 1)))) $ map words linesOfFile

