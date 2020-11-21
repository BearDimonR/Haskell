module Miedviediev06 where

type GraphS = (Int,[(Int,Int)])            
type Graph  = [[Int]]

-- Задача 1 ------------------------------------
isOrdinary :: Graph -> Bool
isOrdinary gr = not (False `elem` [isOrdinaryHelper gr x | x <- [0..(length gr - 1)]])

isOrdinaryHelper :: Graph -> Int -> Bool
isOrdinaryHelper gr i = (length check == length (gr !! i))
 -- && not (null (gr !! i))
 && not (False `elem` [i `elem` (gr !! x) | x<- check, x /= i])
  where check = filter (\x -> x /= i && x < (length gr)) (removeDuplicates (gr !! i))

removeDuplicates :: [Int] -> [Int]
removeDuplicates xs = if null xs then [] else head xs:removeDuplicates (arrayFilter (/= head xs) xs)

arrayFilter :: (Int -> Bool) -> [Int] -> [Int]
arrayFilter p xs = [x | x <- xs, p x]


-- Задача 2 ------------------------------------
fromGraph :: Graph -> GraphS 
fromGraph gr = (len,[(x,y) | x <- [0..len], y <- gr !! x]) where len = length gr - 1


-- Задача 3 ------------------------------------
toGraph :: GraphS -> Graph 
toGraph grS = [[y | (z,y) <- (snd grS), x == z] | x <- [0..len]] where len = fst grS


-- Задача 4 ------------------------------------
--
shortWay :: Graph -> Int -> Int -> [Int]
shortWay gr a b = if null res then [] else reverse (head res)
  where res = checkRes b (head (until (\x -> null ( head x) || not (null(checkRes b (head x)))) (step gr) [[[a]]]))

checkRes :: Int -> [[Int]] -> [[Int]]
checkRes b wss = [ws | ws <- wss, b `elem` ws]

step :: Graph -> [[[Int]]] -> [[[Int]]]
step _ [] = error "step error"
step gr wss@(wsn:_)  = [t:w | w@(x:xs) <- wsn, notElem x xs, t<- gr!!x] : wss

-- Задача 5 ------------------------------------
-- empty arrays?
isConnecting :: Graph -> Bool 
isConnecting gr = [] `notElem` [shortWay gr 0 x | x <- [0..length gr - 1]]

-- Задача 6 ------------------------------------
-- test with empty input [], [[]]
components :: Graph -> [[Int]] 
components gr = componentsHelper gr 0 []

componentsHelper :: Graph -> Int -> [[Int]] -> [[Int]]
componentsHelper gr i res | i == length gr = res
                          | check = componentsHelper gr (i+1) res
                          | otherwise = componentsHelper gr (i+1) (res ++ [cmpnt])
                          where check = True `elem` [i `elem` x | x <- res]
                                cmpnt = [x | x <- [i..length gr - 1], hasWay gr i x]

hasWay :: Graph -> Int -> Int -> Bool
hasWay gr a b = [] /= shortWay gr a b

-- Задача 7 ------------------------------------
eccentricity :: Graph -> Int -> Int 
eccentricity gr a = if null res then (-1) else maximum res - 1
  where res = [length (shortWay gr a x) | x <- [0..length gr - 1]]

-- Задача 8 ------------------------------------
findDiameter :: Graph -> Int 
findDiameter gr = if null gr then 0 else maximum [eccentricity gr x | x <- [0..length gr - 1]]

findRadius :: Graph -> Int 
findRadius gr = if null gr then 0 else minimum [eccentricity gr x | x <- [0..length gr - 1]]


-- Задача 9 ------------------------------------
findCenter :: Graph -> [Int] 
findCenter gr = [x | x <- [0..length gr - 1], eccentricity gr x == r]
  where r = findRadius gr


-- Задача 10 ------------------------------------
shortWays :: Graph -> Int -> Int -> [[Int]] 
shortWays gr a b = map reverse
  (checkRes b (head (until (\x -> null ( head x) || not (null(checkRes b (head x)))) (step gr) [[[a]]])))

---------------------Тестові дані - Графи -------
gr1S, gr2S:: GraphS
gr1S = (5,[(0,1),(0,2),(0,3),(1,0),(1,3),(1,4),
           (2,0),(2,4),(2,5),(3,0),(3,1),(4,1),(4,2),(5,2)])
gr2S = (7,[(0,1),(0,3),(1,0),(1,2),(2,1),(2,3),(3,0),(3,2),
           (4,5),(4,6),(5,4),(5,6), (6,4),(6,5)])

gr1, gr2:: Graph
gr1 = [[1,2,3],[0,3,4],[0,4,5],[0,1],[1,2],[2]]
gr2 = [[1,3],[0,2],[1,3],[0,2],[5,6],[4,6],[4,5],[]]

test :: [Bool]
test = [test1a, test2a, test3a, test4a, test5a, test6a, test7a, test8a, test8aa, test9a, test10a]

-- [True,True,True,True,False,False,True]
test1a :: Bool
test1a = [isOrdinary x | x <- test1b] == [True,True,True,True, True,False,False,True]
test1b :: [[[Int]]]
test1b = [
  [],
  [[]],
  gr1,
  gr2,
  [[1,2], [0], [0]],
  [[1], [0], [0]],
  [[1,2], [0]],
  [[1], [0], []]
  ]

-- [(-1,[]),(0,[]),(2,[(0,1),(0,2),(1,0),(2,0)]),(2,[(0,1),(1,0)]),(2,[(0,1),(1,0),(2,0)])]
test2a :: Bool
test2a = [fromGraph x | x <- test2b] == [(-1,[]),(0,[]),(2,[(0,1),(0,2),(1,0),(2,0)]),(2,[(0,1),(1,0)]),(2,[(0,1),(1,0),(2,0)])]

test2b :: [Graph]
test2b = [
  [],
  [[]],
  [[1,2],[0],[0]],
  [[1], [0], []],
  [[1], [0], [0]]
  ]

 -- [[[]],[[1],[0,2]]]
test3a :: Bool
test3a = [toGraph x | x <- test3b] == [[[]],[[1],[0,2]]]

test3b :: [GraphS]
test3b = [
  (0,[]),
  (1,[(1,0),(0,1),(2,1),(1,2)])
  ]

--[[0,2,5],[0],[5,2,0],[],[5],[],[0],[0]]

test4a :: Bool
test4a = [shortWay x y z | (x, y, z) <- test4b] == [[0,2,5],[0],[5,2,0],[],[5],[],[0],[0]]

test4b :: [(Graph, Int, Int)]
test4b = [
  (gr1, 0, 5),
  (gr1, 0, 0),
  (gr1, 5, 0),
  (gr2, 0, 5),
  (gr2, 5, 5),
  (gr2, 4, 3),
  ([[]], 0, 0),
  ([], 0, 0)
  ]

-- [True,True,True,False,True,False,False]

test5a :: Bool
test5a = [isConnecting x | x <- test5b] == [True,True,True,False,True,False,False]

test5b :: [Graph]
test5b = [
  [],
  [[]],
  gr1,
  gr2,
  [[1,2],[0],[0]],
  [[1], [0], []],
  [[1,2],[0],[0],[4],[3]]
  ]

-- [[],[[0]],[[0,1,2,3,4,5]],[[0,1,2,3],[4,5,6],[7]],[[0,1,2]],[[0,1],[2]],[[0,1,2],[3,4]]]

test6a :: Bool
test6a = [components x | x <- test6b] == [[],[[0]],[[0,1,2,3,4,5]],[[0,1,2,3],[4,5,6],[7]],[[0,1,2]],[[0,1],[2]],[[0,1,2],[3,4]]]

test6b :: [Graph]
test6b =
  [
  [],
  [[]],
  gr1,
  gr2,
  [[1,2],[0],[0]],
  [[1], [0], []],
  [[1,2],[0],[0],[4],[3]]
  ]


-- [-1,0,2,3,1]

test7a :: Bool
test7a = [eccentricity x y | (x,y) <- test7b] == [-1,0,2,3,1]

test7b :: [(Graph,Int)]
test7b =
  [
  ([],0),
  ([[]],0),
  (gr1,0),
  (gr1,5),
  ([[1,2],[0],[0]], 0)
  ]

-- [-1,0,3,2]

test8a :: Bool
test8a = [findDiameter x | x <- test8b] == [0,0,3,2]


-- [-1,0,2,1]
test8aa :: Bool
test8aa = [findRadius x | x <- test8b] == [0,0,2,1]

test8b :: [Graph]
test8b =
  [
  [],
  [[]],
  gr1,
  [[1,2],[0],[0]]
  ]

-- [[],[0],[0,2,4],[0]]

test9a :: Bool
test9a = [findCenter x | x <- test8b] ==  [[],[0],[0,2,4],[0]]

test9b :: [Graph]
test9b =
  [
  [],
  [[]],
  gr1,
  [[1,2],[0],[0]]
  ]

-- [[[1,0,2,5],[1,4,2,5]],[[0,2,5]],[[0]],[[5,2,0]],[],[],[[5]],[],[[0]],[[0]]]

test10a :: Bool
test10a = [shortWays x y z | (x,y,z) <- test10b] == [[[1,0,2,5],[1,4,2,5]],[[0,2,5]],[[0]],[[5,2,0]],[],[],[[5]],[],[[0]],[[0]]]

test10b :: [(Graph,Int,Int)]
test10b =
  [
    (gr1, 1, 5),
    (gr1, 0, 5),
    (gr1, 0, 0),
    (gr1, 5, 0),
    (gr2, 0, 5),
    (gr2, 1, 5),
    (gr2, 5, 5),
    (gr2, 4, 3),
    ([[]], 0, 0),
    ([], 0, 0)
    ]
