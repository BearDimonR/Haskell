module MiedviedievTask where


data AbstractInteger = Zero
                     | Succ AbstractInteger
                     | Pred AbstractInteger
                     deriving (Show, Eq)

type Graph  = [[Int]]

-- Задача 1 -----------------------------------------
instance Ord AbstractInteger where
   (<=) a b = lessEqAbs a b

lessEqAbs :: AbstractInteger -> AbstractInteger -> Bool
lessEqAbs Zero Zero = True
lessEqAbs (Succ _) Zero = False
lessEqAbs (Pred _) Zero = True
lessEqAbs Zero (Succ _) = True
lessEqAbs Zero (Pred _) = False
lessEqAbs (Pred _) (Succ _) = True
lessEqAbs (Succ _) (Pred _) = False
lessEqAbs (Succ a) (Succ b) = lessEqAbs a b
lessEqAbs (Pred a) (Pred b) = lessEqAbs a b

-- Задача 2 ----------------------------------------
aiToInteger :: AbstractInteger -> Integer
aiToInteger Zero = 0
aiToInteger (s@(Succ _)) = case (check s True) of
                    Just n -> n
                    Nothing -> error "Wrong number"

aiToInteger (s@(Pred _)) = case (check s False) of
                    Just n -> n
                    Nothing -> error "Wrong number"

check :: AbstractInteger -> Bool -> Maybe Integer
check a b | b = case a of
                        Zero -> Just 0
                        Succ aa -> case (check aa True) of
                                      Just n -> Just (1 + n)
                                      Nothing -> Nothing
                        Pred _ -> Nothing
          | otherwise = case a of
                        Zero -> Just 0
                        Pred aa -> case (check aa False) of
                                      Just n -> Just (-1 + n)
                                      Nothing -> Nothing
                        Succ _ -> Nothing

-- Задача 3 -----------------------------------------
plusAbs :: AbstractInteger -> AbstractInteger -> AbstractInteger
plusAbs Zero Zero = Zero
plusAbs Zero a = a
plusAbs a Zero = a
plusAbs (Succ a) (Succ b) = Succ (Succ (plusAbs a b))
plusAbs (Pred a) (Pred b) = Pred (Pred (plusAbs a b))
plusAbs (Succ a) (Pred b) = plusAbs a b
plusAbs (Pred a) (Succ b) = plusAbs a b

-- Задача 4 -----------------------------------------
timesAbs :: AbstractInteger -> AbstractInteger -> AbstractInteger
timesAbs Zero _ = Zero
timesAbs a@(Succ aa) b = plusAbs (mult a b) (timesAbs aa b)
timesAbs a@(Pred aa) b = plusAbs (mult a b) (timesAbs aa b)

mult :: AbstractInteger -> AbstractInteger -> AbstractInteger
mult a@(Succ _) (Succ b) = Succ (mult a b)
mult a@(Succ _) (Pred b) = Pred (mult a b)
mult a@(Pred _) (Succ b) = Pred (mult a b)
mult a@(Pred _) (Pred b) = Succ (mult a b)
mult _ Zero = Zero
mult Zero _ = Zero


-- Задача 5 -----------------------------------------
instance Num AbstractInteger  where
    (+)   = plusAbs
    (*)   = timesAbs
    negate      = negateAbs
    fromInteger = fromIntAbs
    abs         = absAbs
    signum      = signumAbs


negateAbs :: AbstractInteger -> AbstractInteger
negateAbs Zero = Zero
negateAbs (Succ a) = Pred (negateAbs a)
negateAbs (Pred a) = Succ (negateAbs a)

fromIntAbs :: Integer -> AbstractInteger
fromIntAbs a | a == 0 = Zero
             | a < 0 = Pred (fromIntAbs (a + 1))
             | otherwise = Succ (fromIntAbs (a - 1))

absAbs :: AbstractInteger -> AbstractInteger
absAbs Zero = Zero
absAbs (Succ a) = Succ (absAbs a)
absAbs (Pred a) = Succ (absAbs a)

signumAbs :: AbstractInteger -> AbstractInteger
signumAbs Zero = 0
signumAbs (Succ _) = 1
signumAbs (Pred _) = -1


test15 :: Bool
test15 = all (== True)
  [
    (<=) Zero Zero == True,
    plusAbs (Pred (Pred Zero))  (Succ (Succ Zero))   == Zero,
    plusAbs (Pred Zero)  (Succ (Succ Zero))   ==  Succ Zero,
    plusAbs (Pred (Pred Zero))  (Pred Zero)   ==  Pred  (Pred  (Pred Zero)),
    plusAbs (Pred (Pred Zero))  Zero   ==  Pred  (Pred Zero),
    plusAbs Zero (Pred (Pred Zero)) ==  Pred  (Pred Zero),
    plusAbs Zero Zero == Zero,
    aiToInteger (Pred (Pred Zero)) == -2,
    aiToInteger Zero == 0,
    aiToInteger (Succ Zero) == 1,
    timesAbs  (Pred (Pred Zero)) (Pred (Pred (Pred Zero))) == Succ( Succ ( Succ (Succ (Succ (Succ Zero))))),
    negate (Succ(Succ Zero)) == (Pred (Pred Zero)),
    negate Zero == Zero,
    negate (Pred (Pred Zero)) == (Succ(Succ Zero)),
    fromInteger 3 == Succ(Succ(Succ Zero)),
    fromInteger 0 == Zero,
    fromInteger (-2) == (Pred (Pred Zero)),
    abs Zero == Zero,
    abs (Succ(Succ Zero)) == (Succ(Succ Zero)),
    abs (Pred (Pred Zero)) == (Succ(Succ Zero)),
    (Succ(Succ Zero)) + (Pred (Pred Zero)) == Zero,
    signum (Pred (Pred Zero)) == -1,
    signum (Succ(Succ Zero)) == 1,
    signum Zero == 0,
    ((Succ(Succ Zero)) > (Succ Zero)) == True,
    (Zero == Zero) == True,
    ((Succ Zero) > (Zero)) == True
  ]


-- Задача 6 -----------------------------------------
gamiltonWay :: Graph -> Maybe [Int]
gamiltonWay gr | null res = Nothing
               | otherwise = Just (reverse (head (tail (res))))
  where res = filter (\x -> head x == last x && checkNode x (nodes gr) && length x > 1) (ways gr)

checkNode :: [Int] -> [Int] -> Bool
checkNode _ [] = True
checkNode arr (n:ns) = n `elem` arr && (checkNode arr ns)

ways :: Graph -> [[Int]]
ways gr = concat $ concat [allWays gr x | x <- nodes gr]

nodes :: Graph -> [Int]
nodes g = [0..(length g - 1)]

allWays :: Graph -> Int -> [[[Int]]]
allWays gr v = until condW (stepW gr) [[[v]]]

condW :: ([[[Int]]]) -> Bool
condW wss = null ( head wss)

stepW :: Graph -> [[[Int]]] -> [[[Int]]]
stepW gr wss@(wsn:_) =
        [t:w | w@(x:xs) <- wsn, notElem x xs, t<- gr!!x] : wss
stepW _ []  = error "step error"

-- Задача  7 -----------------------------------------
isAcyclic :: Graph -> Bool
isAcyclic gr =  null [x | x <- w, length x /= length (removeDuplicates x)]
           where w = ways gr

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs) | n == 0 = newVal:xs
                            | otherwise = x:replaceNth (n-1) newVal xs

-- Задача 8 -----------------------------------------
isTopolSort :: Graph -> [Int] -> Bool
isTopolSort gr tr = isAcyclic gr && (checkTopol gr) == tr

checkTopol :: Graph -> [Int]
checkTopol gr = ((\(_,_,z) -> z) res)
             where res = until (\(_, y, _) -> y >= length gr)
                      (\(x, y, z) -> checkTopolA y (-1) 0 gr x z)
                      ([False | _ <- gr], 0, [])

checkTopolA :: Int -> Int -> Int -> Graph -> [Bool] -> [Int] -> ([Bool], Int, [Int])
checkTopolA n p i gr us rs | us!!n == False = checkTopolA n p i gr (replaceNth n True us) rs
                   | u == (-2) = (us, nn, (n:rs))
                   | us!!u == False = (\(x, _, z) -> checkTopolA n p (i+1) gr x z) (checkTopolA u (-1) 0 gr us rs)
                   | (i+1) >= length g = (us, nn, (n:rs))
                   | otherwise = checkTopolA n p (i+1) gr us rs
                    where u = if i >= length g then (-2) else (g!!i)
                          g = (gr!!n)
                          nn = until (\x -> x >= length gr || us !! x == False) (\x -> x + 1) n


-- Задача 9 -----------------------------------------
longWay :: Graph -> Int -> Int -> Maybe [Int]
longWay gr a b | null r = Nothing
               | otherwise = Just (reverse r)
          where r = foldl (\x y -> if (length x) >= (length y) then x else y) [] w
                w = filter (\x -> head x == b && last x == a && hasDuplic x == False) (ways gr)

hasDuplic :: [Int] -> Bool
hasDuplic a = length a /= length (removeDuplicates a)

---------------------- Графи -------
gr1, gr2, gr3:: Graph
gr1 = [[1,2,3],[2,3],[3,4],[4],[]]
gr2 = [[3,4],[0,3],[0,1,4],[2,4],[1]]
gr3 = [[1],[2],[3],[1],[0,3]]



-- мердж
--- Задача 10 ----------------------------------------

merge :: [Int] -> [Int] -> [Int]
merge a b = removeDuplicates (mergeH a b)

mergeH :: [Int] -> [Int] -> [Int]
mergeH (a:as) (b:bs) | a < b = a : merge as (b:bs)
                    | a == b = a : merge as bs
                    | otherwise = b : merge (a:as) bs
mergeH [] b = b
mergeH a [] = a

removeDuplicates :: [Int] -> [Int]
removeDuplicates xs = if null xs then [] else head xs:removeDuplicates (arrayFilter (/= head xs) xs)

arrayFilter :: (Int -> Bool) -> [Int] -> [Int]
arrayFilter p xs = [x | x <- xs, p x]

-- малі чи великі букви
--- Задача 11 ----------------------------------------
intToString :: Int -> Int -> String
intToString k n | d == 0 = [res]
                | otherwise = intToString d n ++ [res]
          where d = div k n
                m = mod k n
                res = case m of
                  10 -> 'a'
                  11 -> 'b'
                  12 -> 'c'
                  13 -> 'd'
                  14 -> 'e'
                  15 -> 'f'
                  _ -> toEnum (fromEnum '0' + m)

-- change to Int
--- Задача 12 ----------------------------------------
stringToInt :: Int -> String -> Maybe Int
stringToInt _ [] = Nothing
stringToInt n [st] | n <= k = Nothing
                   | otherwise = Just k
                           where k = case st of
                                               'a' -> 10
                                               'b' -> 11
                                               'c' -> 12
                                               'd' -> 13
                                               'e' -> 14
                                               'f' -> 15
                                               _ -> fromEnum st - fromEnum '0'
stringToInt n (st:sts) | n <= k = Nothing
                       | otherwise = case stringToInt n sts of
                                      Just a -> Just (k * n^(length sts) + a)
                                      Nothing -> Nothing
                where k = case st of
                          'a' -> 10
                          'b' -> 11
                          'c' -> 12
                          'd' -> 13
                          'e' -> 14
                          'f' -> 15
                          _ -> fromEnum st - fromEnum '0'

--- Задача 13 ----------------------------------------

type Term = [String]

genExpr :: Int -> Int -> Term
genExpr a b = filter (\x -> eval (tail x) (fromEnum (head x) - fromEnum '0') == b) (build (intToString a 10))

build :: String -> [String]
build [] = []
build s@[_] = [s]
build (st:sts) = if st == '(' || st == ')' then [concat $ [st:x] | x <- res] else concat $ [[st:'+':x, st:'-':x, st:'*':x] | x <- res]
      where res = build sts

eval :: String -> Int -> Int
eval [] r = r
eval (op:st:sts) r = case op of
                        '-' -> eval sts (r - (fromEnum st - fromEnum '0'))
                        '+' -> eval sts (r + (fromEnum st - fromEnum '0'))
                        _ -> eval sts (r * (fromEnum st - fromEnum '0'))
eval _ _ = error "eval wrong"

test1113 :: Bool
test1113 = all (== True)
  [
    intToString 0 3 == "0",
    intToString 543 16  == "21f",
    intToString 543 6 == "2303",
    intToString 5912 8 == "13430",
    intToString 59127 16 == "e6f7",
    stringToInt 10 "56a" == Nothing,
    stringToInt 16 "21f" == Just 543,
    stringToInt 16 "e6f7" == Just 59127,
    stringToInt 8 "13430" == Just 5912,
    stringToInt 2 "2211100" == Nothing,
    genExpr 5432 12 == ["5+4-3*2"],
    genExpr 5432 14 == ["5+4+3+2"]
  ]


--- Задача 14 ----------------------------------------
--genExprBracket :: Int -> Int -> [String]
genExprBracket :: Int -> Int -> [[String]]
genExprBracket a b = undefined


data Op    = Add | Sub | Mul | Div
data Expr  = Val Int | App Op Expr Expr
exp1 :: Expr
exp1 = App Mul(Val 3)(App Sub (App Mul (Val 7)
                 (App Sub (Val 50) (Val 10)))  (Val 25))

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = ":"
instance Show Expr where
  show (Val n) = show n
  show (App op e1 e2) =
              "(" ++ (show e1) ++ show op ++ show e2 ++ ")"


valid :: Op -> Int -> Int -> Bool
valid Sub x y = x > y
valid Div x y  = x `mod` y == 0
valid _ _ _    = True

apply :: Op -> Int -> Int -> Int
apply Add v1 v2 = v1 + v2
apply Sub v1 v2 = v1 - v2
apply Mul v1 v2 = v1 * v2
apply Div v1 v2 = v1 `div` v2

values :: Expr -> [Int]
values (Val n)     = [n]
values (App _ l r) = values l ++ values r

evall :: Expr -> [Int]
evall (Val n) = [n | n>0]
evall (App o l r) = [apply o x y | x <- evall l, y <- evall r, valid o x y]


choices :: [a] -> [[a]]
choices xs = concatMap permutations (subsequences xs)

solution :: Expr -> [Int] -> Int -> Bool 
solution e ns n =  elem (values e) (choices ns)
                          &&  evall e == [n] 






data Tree23 a  = Leaf a
               | Fork2 (Tree23 a) a (Tree23 a)
               | Fork3 (Tree23 a) a (Tree23 a) a (Tree23 a)
               | Null23     -- порожнє 2-3-дерево!!!
               deriving (Eq, Show)


--- Задача 15 ----------------------------------------
isTree23  :: (Ord a) => Tree23 a -> Bool 
isTree23 Null23 = True
isTree23 (Leaf _) = True
isTree23 (Fork2 tl x tr) = isTree23H tl && isTree23H tr && (checkLeft tl x) && (minTr tr == x) && (checkH tl == checkH tr)
isTree23 (Fork3 tl x tm y tr) = isTree23H tl && isTree23H tm && isTree23H tr &&
                                (checkLeft tl x) && (minTr tm == x) &&
                                (checkLeft tm y) && (minTr tr == y) &&
                                (checkH tl == checkH tr) && (checkH tl == checkH tm)

isTree23H  :: (Ord a) => Tree23 a -> Bool
isTree23H Null23 = False
isTree23H (Leaf _) = True
isTree23H (Fork2 tl x tr) = (checkLeft tl x) && (minTr tr == x) && (checkH tl == checkH tr) && isTree23H tl && isTree23H tr
isTree23H (Fork3 tl x tm y tr) = (checkLeft tl x) && (minTr tm == x) &&
                                (checkLeft tm y) && (minTr tr == y) &&
                                (checkH tl == checkH tr) && (checkH tl == checkH tm) && isTree23H tl && isTree23H tm && isTree23H tr

checkLeft :: (Ord a) => Tree23 a -> a -> Bool
checkLeft Null23 _ = False
checkLeft (Leaf k) x = k <= x
checkLeft (Fork2 tl k tr) x = k <= x && checkLeft tl x  && checkLeft tr x
checkLeft (Fork3 tl kl tm kr tr) x = kl <= x && kr <= x && checkLeft tl x && checkLeft tr x && checkLeft tm x

minTr :: (Ord a) => Tree23 a -> a
minTr Null23 = error "Can't be minTr"
minTr (Leaf k) = k
minTr (Fork2 tl _ tr) = min (minTr tl) (minTr tr)
minTr (Fork3 tl _ tm _ tr) = min (min (minTr tl) (minTr tr)) (minTr tm)

checkH :: (Ord a) => Tree23 a -> Int
checkH Null23 = error "Can't be checkH"
checkH (Leaf _) = 1
checkH (Fork2 tl _ _) = 1 + checkH tl
checkH (Fork3 tl _ _ _ _) = 1 + checkH tl

--- Задача 16 ----------------------------------------
eqTree23 :: (Ord a) => Tree23 a -> Tree23 a -> Bool
eqTree23 t1 t2 = findN t1 == findN t2

findN :: (Ord a) => Tree23 a -> [a]
findN Null23 = []
findN (Leaf k) = [k]
findN (Fork2 tl _ tr) = findN tl ++ findN tr
findN (Fork3 tl _ tm _ tr) = findN tl ++ findN tm ++ findN tr

--- Задача 17 ----------------------------------------

elemTree23 :: (Ord a) => Tree23 a -> a -> Bool
elemTree23 Null23 _ = False
elemTree23 (Leaf k) x = k == x
elemTree23 (Fork2 tl k tr) x | k > x = elemTree23 tl x
                             | otherwise = elemTree23 tr x
elemTree23 (Fork3 tl kl tm kr tr) x | kr > x && x >= kl = elemTree23 tm x
                                    | kl > x = elemTree23 tl x
                                    | otherwise = elemTree23 tr x

--- Задача 18 ----------------------------------------
insTree23 :: (Ord a) => Tree23 a -> a -> Tree23 a
insTree23 t v  = let (rtr, res) = insert v t
                  in case res of
                      Just (minA, jtr) -> Fork2 rtr minA jtr
                      Nothing -> rtr

-- isTerminal tr = True <=> якщо сини вузла tr - листки !!
isTerminal :: Tree23 a -> Bool
isTerminal (Fork2 (Leaf _) _ _)     = True 
isTerminal (Fork3 (Leaf _) _ _ _ _) = True
isTerminal _                        = False

-- Результат вставки вузла в 2-3-дерево, 
--   корінь якого - вузол вида Fork2 або Fork3 є об`єкт із (Tree23 a, Maybe (a, Tree23 a))
--   : (a, Nothing) - результат вставки - одне 2-3-дерево a 
--   : (a, Just (w, b)) - результат вставки два 2-3-дерева a i b (w - найменше значення в b)
--  insert v tr - додає значення v в довільне дерево tr
insert :: (Ord a) => a -> Tree23 a -> (Tree23 a, Maybe (a, Tree23 a))
insert v tr | isTerminal tr = insTerm v tr
            | otherwise     = insFork v tr

-- insTerm v tr - додається значення v в дерево tr з конем - термінальний вузол 
insTerm :: (Ord a) => a -> Tree23 a -> (Tree23 a, Maybe (a, Tree23 a))
insTerm x (Fork2 ll@(Leaf l) k lr@(Leaf r)) | x < l = (Fork3 (Leaf x) l ll k lr , Nothing)
                                            | l <= x && x <= r = (Fork3 ll x (Leaf x) k lr , Nothing)
                                            | otherwise = (Fork3 ll k lr x (Leaf x) , Nothing)
insTerm x (Fork3 ll@(Leaf l) kl lm@(Leaf m) kr lr@(Leaf r))
                        | x < l = ((Fork2 (Leaf x) l ll), Just (m ,(Fork2 lm kr lr)))
                        | l <= x && x < m = ((Fork2 ll x (Leaf x)), Just (m ,(Fork2 lm kr lr)))
                        | m <= x && x <= r = ((Fork2 ll kl lm), Just (x ,(Fork2 (Leaf x) kr lr)))
                        | otherwise = ((Fork2 ll kl lm), Just (r ,(Fork2 lr x (Leaf x))))
insTerm _ _ = error "Insert term error"

-- insFork v tr - додає значення v в дерево tr з корнем - нетермінальний вузол 
insFork :: (Ord a) => a -> Tree23 a -> (Tree23 a, Maybe (a, Tree23 a))
insFork a Null23 = ((Leaf a), Nothing)
insFork a ll@(Leaf l) | a < l = ((Fork2 (Leaf a) l ll), Nothing)
                      | otherwise = ((Fork2 ll a (Leaf a)), Nothing)
insFork a (Fork2 tl k tr) | a < k = let res = insert a tl
                                in case res of
                                    (ttr@(Fork2 _ _ _), mbtr) -> case mbtr of
                                                                        Just (aa, jtr) -> (Fork3 ttr aa jtr k tr, Nothing)
                                                                        Nothing -> (Fork2 ttr k tr, Nothing)
                                    (resTr@(Fork3 _ _ _ _ _), mbtr) -> case mbtr of
                                                                               Just _ -> error "fork3 ins fork error"
                                                                               Nothing -> (Fork2 resTr k tr, Nothing)
                                    _ -> error "insFork error"
                          | otherwise = let res = insert a tr
                                in case res of
                                    (resTr@(Fork2 _ _ _), mbtr) -> case mbtr of
                                                                    Just (minA, jtr) -> (Fork3 tl k resTr minA jtr, Nothing)
                                                                    Nothing -> ((Fork2 tl k resTr), Nothing)
                                    (resTr@(Fork3 _ _ _ _ _), mbtr) -> case mbtr of
                                                                                 Just _ -> error "fork3 ins fork error"
                                                                                 Nothing -> (Fork2 tl k resTr, Nothing)
                                    _ -> error "insFork error"



insFork a (Fork3 tl kl tm kr tr) | a < kl = let res = insert a tl
                                            in case res of
                                                  (resTr@(Fork2 _ _ _), mbtr) -> case mbtr of
                                                                                  Just (minA, jtr) -> (Fork2 resTr minA jtr, Just (kl, (Fork2 tm kr tr)))
                                                                                  Nothing -> ((Fork3 resTr kl tm kr tr),Nothing)
                                                  (resTr@(Fork3 _ _ _ _ _), mbtr) -> case mbtr of
                                                                                        Just _ -> error "fork3 ins fork error"
                                                                                        Nothing -> ((Fork3 resTr kl tm kr tr),Nothing)
                                                  _ -> error "insFork error"

                                 | kl <= a && a <= kr = let res = insert a tm
                                                         in case res of
                                                              (resTr@(Fork2 _ _ _), mbtr) -> case mbtr of
                                                                                               Just (minA, jtr) -> (Fork2 tl kl resTr, Just (minA, (Fork2 jtr kr tr)))
                                                                                               Nothing -> (Fork3 tl kl resTr kr tr,Nothing)
                                                              (resTr@(Fork3 _ _ _ _ _), mbtr) -> case mbtr of
                                                                                                   Just _ -> error "fork3 ins fork error"
                                                                                                   Nothing -> (Fork3 tl kl resTr kr tr,Nothing)
                                                              _ -> error "insFork error"
                                 | otherwise = let res = insert a tr
                                                 in case res of
                                                      (resTr@(Fork2 _ _ _), mbtr) -> case mbtr of
                                                                                      Just (minA, jtr) -> (Fork2 resTr minA jtr, Just (kl, (Fork2 tm kr tr)))
                                                                                      Nothing -> ((Fork3 tl kl tm kr resTr),Nothing)
                                                      (resTr@(Fork3 _ _ _ _ _), mbtr) -> case mbtr of
                                                                                           Just _ -> error "fork3 ins fork error"
                                                                                           Nothing -> ((Fork3 tl kl tm kr resTr),Nothing)
                                                      _ -> error "insFork error"



test1518 :: Bool
test1518 = all (== True)
  [
    isTree23 (Fork2 (Leaf 0) 1 (Leaf 2)) == False,
    isTree23 tr1 == True,
    isTree23 tr2 == True,
    isTree23 tr3 == True,
    eqTree23 tr1 tr2 == True,
    eqTree23 (insTree23 tr3 18) tr4 == True,
    elemTree23 tr3 12 == True,
    elemTree23 tr1 0 == True,
    elemTree23 tr1 1 == True,
    elemTree23 tr1 2 == True,
    elemTree23 tr1 3 == True,
    elemTree23 tr1 4 == True,
    elemTree23 tr1 5 == True,
    elemTree23 tr1 6 == True,
    elemTree23 tr1 7 == True,
    elemTree23 tr1 8 == False,
    elemTree23 tr1 12 == False,
    elemTree23 tr3 2 == True,
    elemTree23 tr3 5 == True,
    elemTree23 tr3 7 == True,
    elemTree23 tr3 8 == True,
    elemTree23 tr3 12 == True,
    elemTree23 tr3 16 == True,
    elemTree23 tr3 19 == True,
    elemTree23 tr3 13 == False,
    insTree23 tr4 10 == tr5,
    insTree23 Null23 1 == (Leaf 1)
  ]


---------------------Тестові дані

---------------------- 2-3-дерева
tr1, tr2, tr3, tr4,tr5 :: Tree23 Int
tr1 =  Fork2 (Fork2 (Fork2 (Leaf 0) 1 (Leaf 1)) 
                     2
                    (Fork2 (Leaf 2) 3 (Leaf 3)))
              4
             (Fork2 (Fork2 (Leaf 4) 5 (Leaf 5)) 
                     6
                    (Fork2 (Leaf 6) 7 (Leaf 7)))
tr2 =  Fork3 (Fork2 (Leaf 0) 1 (Leaf 1))
              2
             (Fork3 (Leaf 2) 3 (Leaf 3) 4 (Leaf 4))
              5
             (Fork3 (Leaf 5) 6 (Leaf 6) 7 (Leaf 7))

tr3 = Fork3 (Fork2 (Leaf 2) 5 (Leaf 5))
            7
            (Fork3 (Leaf 7) 8 (Leaf 8) 12 (Leaf 12))
            16
            (Fork2 (Leaf 16) 19 (Leaf 19))

tr4 = Fork3 (Fork2 (Leaf 2) 5 (Leaf 5))
            7
            (Fork3 (Leaf 7) 8 (Leaf 8) 12 (Leaf 12))
            16
            (Fork3 (Leaf 16) 18 (Leaf 18) 19 (Leaf 19))

tr5 = Fork2 (Fork2 (Fork2 (Leaf 2) 5 (Leaf 5))
                    7
                   (Fork2 (Leaf 7) 8 (Leaf 8)) 
            )
            10
            (Fork2 (Fork2 (Leaf 10) 12 (Leaf 12))
                   16
                   (Fork3 (Leaf 16) 18 (Leaf 18) 19 (Leaf 19))
            )