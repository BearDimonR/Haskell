module Miedviediev02 where

-- Task 1 -----------------------------------------
sumFl :: [Integer] -> Integer
sumFl = foldl (+) 0
  
-- Task 2 -----------------------------------------
productFr :: [Integer] -> Integer
productFr xs
  | null xs = 0
  | otherwise = foldl (*) 1 xs

-- Task 3 -----------------------------------------
concatFr :: [Int] -> [Int] -> [Int]
concatFr xs ys  = foldr (:) ys xs


-- Task 4 -----------------------------------------
sortInsert :: [Int] -> [Int]
sortInsert  = foldl insert []

insert :: [Int] -> Int -> [Int]
insert [] y = [y]
insert (x:xs) y
  | y < x = y : x : xs
  | otherwise = x : insert xs y

-- Task 5 -----------------------------------------
findIndices ::(Int -> Bool) -> [Int] -> [Int] 
findIndices p xs = findIndicesHelper p xs 0

findIndicesHelper ::(Int -> Bool) -> [Int] -> Int -> [Int]
findIndicesHelper _ [] _ = []
findIndicesHelper m (y:ys) n
            | m y = n : findIndicesHelper m ys (n + 1)
            | otherwise = findIndicesHelper m ys (n + 1)

-- Task 6 -----------------------------------------
allReverse :: [String] -> [String]
allReverse xs = map reverse (reverse xs)

-- Task 7  -----------------------------------------
noDigits :: String -> String
noDigits = filter (\x -> not (x `elem` "123456789"))

-- Task 8 ------------------------------------------
cntGood :: [Int -> Bool] -> Int -> Int
cntGood xs n = length [x | x <- xs, x n]

-- Task 9 ------------------------------------------
trianglePas :: [[Integer]]
trianglePas = [scanl (\y z -> div (y * (x - z)) z) 1 [1..(x-1)] | x <- [1..]]

-- Task 10 -----------------------------------------
factorialsM :: [Integer]
factorialsM = 1 : zipWith (*) factorialsM [2..]
