module Miedviediev07 where

import Data.List


data BinTreeM a = EmptyM 
                | NodeM a Int (BinTreeM a) (BinTreeM a)
                   deriving (Show, Eq) 
-- B-дерево порядка t (NodeB kl tl) =>  
--      t-1 <= length kl <= 2*t-1  &&  t <= length tl <= 2*t
data Btree a =  NodeB [a] [Btree a]  deriving (Show, Eq)
-- головні характеристики B-дерево  (BInform height min max)
data BInform a = BInform {hB::Int, minB::a, maxB::a} deriving (Show, Eq)

-- Задача 1 ------------------------------------
isSearch :: (Ord a) => BinTreeM a -> Bool
isSearch a = case a of
    EmptyM -> False
    NodeM q k tl tr -> (k > 0) && (tl == EmptyM || q > (getQ tl)) && (tr == EmptyM || q < (getQ tr))

getQ :: BinTreeM a -> a
getQ a = case a of
    EmptyM -> error "getQ error"
    NodeM q _ _ _ -> q

-- Задача 2 ------------------------------------
elemSearch :: (Ord a) => BinTreeM a -> a -> Bool
elemSearch a b = case a of
    EmptyM -> False
    NodeM q _ tl tr -> if q == b then True else
              if q < b then elemSearch tr b else elemSearch tl b

-- Задача 3 ------------------------------------
insSearch :: (Ord a) => BinTreeM a -> a -> BinTreeM a 
insSearch a val = case a of
    EmptyM -> NodeM val 1 EmptyM EmptyM
    NodeM q k tl tr -> if q == val then NodeM q (k + 1) tl tr
        else if q < val then NodeM q k tl (insSearch tr val)
        else NodeM q k (insSearch tl val) tr

-- Задача 4 ------------------------------------
delSearch :: (Ord a) => BinTreeM a -> a -> BinTreeM a 
delSearch a val = delSearchHelper a val False

delSearchHelper :: (Ord a) => BinTreeM a -> a -> Bool -> BinTreeM a
delSearchHelper a val p = case a of
      EmptyM -> EmptyM
      NodeM q k tl tr -> if q == val then deleteNode q k tl tr p
         else if q < val then NodeM q k tl (delSearch tr val)
         else NodeM q k (delSearch tl val) tr

deleteNode :: (Ord a) => a -> Int -> BinTreeM a -> BinTreeM a -> Bool -> BinTreeM a
deleteNode q k tl tr p | p || k > 1 = NodeM q (k - 1) tl tr
                       | tl == EmptyM = tr
                       | tr == EmptyM = tl
                       | otherwise = case na of
                          EmptyM -> error "deleteNode error"
                          NodeM nq nk _ _ -> NodeM nq nk tl (delSearchHelper tr nq True)
                     where na = findMin tr
findMin :: (Ord a) => BinTreeM a -> BinTreeM a
findMin a = case a of
    EmptyM -> EmptyM
    NodeM _ _ tl _ -> if tl == EmptyM then a else findMin tl


-- Задача 5 ------------------------------------
sortList :: (Ord a) => [a] -> [a]
sortList a = sortListHelper (foldl insSearch EmptyM a) []

sortListHelper :: (Ord a) => BinTreeM a -> [a] -> [a]
sortListHelper a b = case (findMin a) of
    EmptyM -> b
    NodeM q _ _ _ -> sortListHelper (delSearch a q) (b ++ [q])

-- Задача 6 ------------------------------------
findBInform :: (Bounded a, Ord a) => Btree a ->  BInform a
findBInform a = BInform (findH a) (findMn a) (findMx a)

findH :: (Bounded a, Ord a) => Btree a -> Int
findH (NodeB _ bs) | null bs = 0
                    | otherwise = 1 + maximum [findH x | x <- bs]

findMn :: (Bounded a, Ord a) => Btree a -> a
findMn (NodeB as bs) | null bs = mn
                     | otherwise = min mn (minimum [findMn x | x <- bs])
                     where mn = minimum as

findMx :: (Bounded a, Ord a) => Btree a -> a
findMx (NodeB as bs) | null bs = mx
                     | otherwise = max mx (maximum [findMx x | x <- bs])
                     where mx = maximum as

-- Задача 7 ------------------------------------
isBtree  :: (Bounded a, Ord a) => Int -> Btree a -> Bool
isBtree t (NodeB as bs) | sort as /= as = False
                        | null bs && null as = True
                        | null bs = core
                        | otherwise = core && (length bs == n + 1) && (checkSiblings (findH (NodeB as bs)) as bs) &&
                         (foldl (\x y -> x && isBtreeNode t y) True bs)
                        where core = n >= 1 && n <= 2*t-1
                              n = length as

isBtreeNode :: (Bounded a, Ord a) => Int -> Btree a -> Bool
isBtreeNode t (NodeB as bs) | sort as /= as = False
                            | null bs = core
                            | otherwise = core && (length bs == n + 1) && (checkSiblings (findH (NodeB as bs)) as bs) &&
                                 (foldl (\x y -> x && isBtreeNode t y) True bs)
                                 where core = n >= (t-1) && n <= 2*t-1
                                       n = length as


checkSiblings :: (Bounded a, Ord a) => Int -> [a] -> [Btree a] -> Bool
checkSiblings h as bs = False `notElem` is
                      where is = [checkSibling h x as (findBInform (head (drop x bs))) | x <- [0..length bs-1]]

checkSibling :: (Ord a) => Int -> Int -> [a] -> BInform a -> Bool
checkSibling h i as (BInform nh nmn mnx) | i == 0 = (h == nh + 1) && head as > mnx
                                        | i == length as = (h == nh + 1) && last as < nmn
                                        | otherwise = (h == nh + 1)
                                        && head (drop i as) > mnx
                                        && head (drop (i-1) as) < nmn

-- Задача 8 ------------------------------------
eqBtree :: (Bounded a, Ord a) => Int -> Btree a -> Btree a -> Bool 
eqBtree t (NodeB as bs) (NodeB cs ds) | null as || length as > 2*t-1 || null cs || length cs > 2*t-1 = False
                                      | otherwise = (checkT t bs) && (checkT t ds)
                                      && checkInside (NodeB as bs) == checkInside (NodeB cs ds)

checkT :: (Bounded a, Ord a) => Int -> [Btree a] -> Bool
checkT t xs = foldl (\ x (NodeB as bs)-> x && length as >= t - 1 && length as <= 2*t-1 && checkT t bs) True xs

checkInside :: (Bounded a, Ord a) => Btree a -> [a]
checkInside (NodeB as bs) | null bs = as
                          | otherwise = foldl (\x y -> x ++ formArray y as bs) [] [0..length as]

formArray :: (Bounded a, Ord a) => Int -> [a] -> [Btree a] -> [a]
formArray i as bs | i == 0 = checkInside (head bs)
                  | otherwise = head (drop (i - 1) as) : checkInside (head (drop i bs))


-- Задача 9 ------------------------------------
elemBtree :: Ord a => Btree a -> a -> Bool
elemBtree (NodeB as bs) a | null bs = check
                          | otherwise = check || foldl (\x y -> x || elemBtree y a) False bs
                          where check = (position a as) /= -1

position :: Ord a => a -> [a] -> Int
position _ [] = -1
position b (a:as) | b == a = 0
                | b `notElem` as = -1
                | otherwise = 1 + (position b as)

-- Задача 10 ------------------------------------
insBtree :: Ord a => Int -> Btree a -> a -> Btree a
insBtree t bt@(NodeB as bs) v | isFull t bt = insBtree t (formB (splitAtB t bt)) v
                              | null bs = NodeB (insertKey v as) []
                              | isFull t (bs !! pos) = splitFull t bt pos v
                              | otherwise = NodeB as ((take pos bs) ++ ((insBtree t (bs!!pos) v):(drop (pos+1) bs)))
                              where pos = (position2 v as)

splitFull :: Ord a => Int -> Btree a -> Int -> a -> Btree a
splitFull t (NodeB as bs) pos v = NodeB ((take (pos) as) ++ [b] ++ (drop (pos) as))
                              ((take pos bs) ++ res ++ (drop (pos+1) bs))
        where (a,b,c) = splitAtB t (bs !! pos)
              res = if v < b then [insBtree t a v,c] else [a,insBtree t c v]

isFull :: Int -> Btree a -> Bool
isFull t (NodeB as _) = length as == 2*t-1

insertKey :: Ord a => a -> [a] -> [a]
insertKey a as = sort (a : as)

position2 :: Ord a => a -> [a] -> Int
position2 _ [] = 0
position2 v (x:xs) | v <= x = 0
                  | otherwise = 1 + position2 v xs


--decomposeNodeB :: Ord a => a -> [a] -> [Btree a] ->
--                        ([a], [a], [Btree a], Btree a, [Btree a])
-- decomposeNodeB = undefined

formB :: (Btree a, a, Btree a) -> Btree a
formB (a, mid, b) =  NodeB [mid] [a,b]

splitAtB :: Int -> Btree a -> (Btree a, a, Btree a)
splitAtB t (NodeB kl tl) = ((NodeB (take mid kl) (take t tl)),kl!!mid,(NodeB (drop (mid+1) kl) (drop t tl))) where mid = (t-1)

---------------------Тестові дані - Дерева пошуку -------
bm :: BinTreeM Char
bm = NodeM  't' 2  
            (NodeM 'a' 1  EmptyM 
                    (NodeM 'e' 1 
                             (NodeM 'd' 2 EmptyM EmptyM)
                             (NodeM 'f' 1 EmptyM EmptyM)
                    )
            ) 
            (NodeM 'w' 2  EmptyM EmptyM)   

tBt1 :: Btree Char 
tBt1 = NodeB "L"
       [ NodeB "DG" 
          [ NodeB "AC" [], NodeB "EE" [], NodeB "HK" []
          ]
       , NodeB "PU" 
          [ NodeB "MM" [], NodeB "RS" [], NodeB "UW" []
          ]
       ]

tBt2 :: Btree Char 
tBt2 = NodeB "GP"
       [ NodeB "ACDEE" [], NodeB "HKLMM" [], NodeB "RSUUW" []
       ]

tBt5 :: Btree Char 
tBt5 = NodeB "GMPX"
       [ NodeB "ACDE" [] , NodeB "JK" [], NodeB "NO" []
       , NodeB "RSTUV" [], NodeB "YZ" []
       ]

tBt6 :: Btree Char 
tBt6 = NodeB "GMPX"
       [ NodeB "ABCDE" [], NodeB "JK" [], NodeB "NO" []
       , NodeB "RSTUV" [], NodeB "YZ" []
       ]

tBt7 :: Btree Char 
tBt7 = NodeB "GMPTX"
       [ NodeB "ABCDE" [], NodeB "JK" [], NodeB "NO" []
       , NodeB "QRS" [], NodeB "UV" [], NodeB "YZ" []
       ]

tBt8 :: Btree Char 
tBt8 = NodeB "P"
       [ NodeB "GM"
          [ NodeB "ABCDE" [], NodeB "JKL" [], NodeB "NO" []
          ]
       , NodeB "TX" 
          [ NodeB "QRS" [], NodeB "UV" [], NodeB "YZ" []
          ]
       ]

tBt9 :: Btree Char 
tBt9 = NodeB "P"
       [ NodeB "CGM"
          [ NodeB "AB" [], NodeB "DEF" []
          , NodeB "JKL" [], NodeB "NO" []
          ]
       , NodeB "TX" 
          [ NodeB "QRS" [], NodeB "UV" [], NodeB "YZ" []
          ]
       ]
