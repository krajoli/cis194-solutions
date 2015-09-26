{-# OPTIONS_GHC -Wall #-}
module HW02 where

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches c1 c2 = length $ filter (== True) $ zipWith (==) c1 c2

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors code = map (\c -> count c code) colors
            where count c d = length $ filter (==c) d

-- Count number of matches between the actual code and the guess
matchesOld :: Code -> Code -> Int
matchesOld c1 c2 = length $ filter (>0) $ zipWith min (countColors c1) (countColors c2)

matches :: Code -> Code -> Int
matches c1 c2 = sum $ zipWith min (countColors c1) (countColors c2)

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove c1 c2 = Move c2 em (m - em)
        where em = exactMatches c1 c2
              m = matches c1 c2

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent move@(Move secret _ _) guess = getMove guess secret == move

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes m c = filter (isConsistent m) c

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes 0 = []
allCodes 1 = [[Red],[Green],[Blue],[Yellow],[Orange],[Purple]]
allCodes n = concatMap (\x -> map (x++)  $ allCodes (n-1)) $ allCodes 1

-- Exercise 7 -----------------------------------------

sol :: [Move] -> [Move] -> [Move]
sol result [] = result
sol result (move@(Move x e _):xs) 
    | e == length x = move : result
    | otherwise = sol (move : result) xs

solve :: Code -> [Move]
solve c = sol [initialMove] $ map (getMove c) $ allCodes $ length c
      where initialMove = getMove c (take 5 $ repeat Red)

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined
