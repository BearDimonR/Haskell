module Miedviediev01 where

-- Task 1 -----------------------------------------
------- func make infinitive array of cubed natural number ------
power3 :: [Integer]
power3 = [x ^ (3 :: Integer) | x <- [1..]]

-- Task 2 -----------------------------------------
------- func make infinitive array of powers 3 ------
toPower3 :: [Integer]
toPower3 = [(3 :: Integer) ^ x | x <- [(1::Integer)..]]

-- Task 3 -----------------------------------------
------- func counts sum of powers 3 from 1 to n ------
sumPower3 :: Integer -> Integer
sumPower3 n = sum [3 ^ x | x <- [1..n]]
-- sumPower3 n = if n == 1 then 3 else (n ^ (3 :: Integer)) + sumPower3 (n - 1) --

-- Task 4 -----------------------------------------
------- func counts sum of powers m from power 1 to n -------
sumPower :: Integer -> Integer -> Integer
sumPower m n = sum [m ^ i | i <- [1..n]]

-- Task 5 -----------------------------------------
------- func returns array with numbers how many  less number in input array  -------
lessMe :: [Int] -> [Int]
lessMe xs = [countLess xs i | i <- xs]

countLess :: [Int] -> Int -> Int
countLess xs n |
 null xs = 0 |
  head xs < n = 1 + countLess (tail xs) n |
   otherwise = countLess (tail xs) n

-- select from array by index
--select :: [Int] -> Int -> Int
--select xs i = if i == 0 then head xs else select (tail xs) (i - 1)
 
-- Task 6 -----------------------------------------
------- func returns array with frequency numbers of initial array -------
frequency :: [Int] -> [(Int,Int)]
frequency xs = [(i, countFrequency xs i) | i <- removeDuplicates xs]

countFrequency :: [Int] -> Int -> Int
countFrequency xs n |
 null xs = 0 |
  head xs == n = 1 + countFrequency (tail xs) n |
    otherwise = countFrequency (tail xs) n

removeDuplicates :: [Int] -> [Int]
removeDuplicates xs = if null xs then [] else head xs:removeDuplicates (arrayFilter (/= head xs) xs)

arrayFilter :: (Int -> Bool) -> [Int] -> [Int]
arrayFilter p xs = [x | x <- xs, p x]

-- Task 7 -----------------------------------------
------- func returns next number of gradiny-numbers -------
hailstone :: Int -> Int
hailstone n = if n <= 1 then error "invalid number!" else hailstoneCalc n

hailstoneCalc :: Int -> Int
hailstoneCalc n | n == 1 = 0 | even n = div n 2 | otherwise = n * 3 + 1

-- Task 8 -----------------------------------------
------- func returns array of gradiny-numbers starting from n -------
hailSeq :: Int -> [Int]
hailSeq n | n == 0 = error "invalid number" | hailstoneCalc n == 0 = [1] | otherwise =  n : hailSeq (hailstoneCalc n)

-- Task 9 -----------------------------------------
------- func returns infinitive array of arrays of gradiny-numbers -------
allHailSeq :: [[Int]]
allHailSeq = [hailSeq i | i <- [1..], i /= 0]

-- Task 10 -----------------------------------------
------- func returns first number starting from which array of gradiny-numbers length == n -------
firstHailSeq :: Int -> Int
firstHailSeq n = if n == 0 then error "invalid number" else firstHailSeqRec n 1

firstHailSeqRec :: Int -> Int -> Int
firstHailSeqRec n i = if hailLen (hailSeq i) == n then i else firstHailSeqRec n (i + 1)

hailLen :: [Int] -> Int
hailLen xs = if null xs then 0 else 1 + hailLen (tail xs) 

