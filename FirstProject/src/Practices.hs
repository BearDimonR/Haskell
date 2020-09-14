{-# OPTIONS_GHC -Wall #-}
module Practices where

foo :: [Int] -> [Int]
foo xs = map (cntMore xs) xs

cntMore :: [Int] -> Int -> Int
cntMore xs x = length (filter (x<) xs)
