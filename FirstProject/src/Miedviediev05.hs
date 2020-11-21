module Miedviediev05 where

import Data.Char(isUpper)
import Data.List

type Grammar    = [Production]         -- КВ-граматика
type Production = (Char,String)        -- Правило виводу
type Predict    = [(Char,String)]      -- Прогнозуюча таблиця
type Control    = [((Char,Char),Int)]  -- Управляюча таблиця 

-- Задача 1 ------------------------------------
addOne :: String -> Char -> String  
addOne xs x = sort (if x `notElem` xs then xs ++ [x] else xs)

addAll :: String -> String -> String
addAll = foldl addOne
addWithout :: String -> String -> String 
addWithout xs ys = addAll xs (filter (/= '$') ys)

inter :: String -> String -> String 
inter xs = nub. filter (`elem` xs) .sort

-- Задача 2 ------------------------------------
tkPredict :: Predict -> Char -> String
tkPredict [] _ = ""
tkPredict ((z,y):xs) x = if x == z then y else tkPredict xs x

upPredict :: Predict -> Char -> String -> Predict
upPredict [] n st = [(n, st)]
upPredict ((z,y):xs) n st = if n == z then (z, st) : xs else (z,y) : upPredict xs n st

-- Задача 3 ------------------------------------
parse ::  Grammar -> Control -> String -> Maybe [Int]
parse gm ctr w = (\(_, _, z) -> z) (step gm ctr (w++['$'], "S$", Just []))

-- what about ""
-- TODO CHECK
step :: Grammar -> Control -> 
       (String, String, Maybe [Int]) -> (String, String, Maybe [Int])
step gm ctr (w:wd, s:st, res) | s == w = step gm ctr (wd, st, res)
                              | null g && null m = ("", "$", Nothing)
                              | null g = case res of
                                          Just r -> step gm ctr (w:wd, st, Just (r ++ [snd (head m)]))
                                          Nothing -> ("$", "", Nothing)
                              | otherwise = case res of
                                              Just r -> step gm ctr (w:wd, snd (gm !! snd (head g))++st, Just (r ++ [snd (head g)]))
                                              Nothing -> ("$", "", Nothing)
  where g = filter (\x -> fst x == (s, w)) ctr
        m = filter (\x -> fst x == (s, '$')) ctr
step _ _ ([], [], res) = ("", "", res)
step _ _ (_, [], _) = ("", "", Nothing)
step gm ctr ([], s:st, res) | null m = ("$", "", Nothing)
                            | otherwise = case res of
                                Just r -> step gm ctr ([], st, Just (r ++ [snd (head m)]))
                                Nothing -> ("$", "", Nothing)
  where m = filter (\x -> fst x == (s, '$')) ctr




isPrefix :: Char -> String -> Bool
isPrefix b xs = b == head xs

-- Задача 4 ------------------------------------
first :: Predict -> String -> String
first _ [] = "$"
first pr (y:ys) | isUpper y && '$' `elem` snd res = addAll (addWithout [] (snd res)) (first pr ys)
                | isUpper y = snd res
                | otherwise = [y]
  where res = head (filter (\(x,_) -> x == y) pr)

-- Задача 5 ------------------------------------
buildingControl :: Grammar -> Predict -> Predict -> Control
buildingControl gm pr1 pr2 = sortBy sortControls (buildingControlHelper 0 gm pr1 pr2)

sortControls :: ((Char, Char), Int) -> ((Char, Char), Int) -> Ordering
sortControls ((a,b),_) ((c,d),_) | a > c = GT
                                 | a == c && b > d = GT 
                                 | otherwise = LT

sortPredicts :: (Char, String) -> (Char, String) -> Ordering
sortPredicts (a,b) (c,d) | a > c = GT
                         | a == c && b > d = GT
                         | otherwise = LT

buildingControlHelper :: Int -> Grammar -> Predict -> Predict -> Control
buildingControlHelper _ [] _ _ = []
buildingControlHelper i (g:gm) fst0 nxt0
  | '$' `elem` res = [((fst g, b), i) | b <- snd bs] ++ buildingControlHelper (i+1) gm fst0 nxt0
  | otherwise = [((fst g, a), i) | a <- as] ++ buildingControlHelper (i+1) gm fst0 nxt0
  where res = first fst0 (snd g)
        as = filter (not . isUpper) res
        bs = head [x | x <- nxt0, fst x == fst g]

-- Задача 6 ------------------------------------
testingLL1 :: Grammar -> Predict -> Predict -> Bool
testingLL1 g pFst pNxt = foldl
  (\x y -> x
  && testFst pFst (snd y)
  && testFollow pFst (snd (selectN pNxt (fst y))) (snd y)) True (fromGrammar g)

fromGrammar :: Grammar ->  [(Char,[String])]
fromGrammar g = fromGrammarHelper g []

fromGrammarHelper :: Grammar -> [(Char,[String])] ->  [(Char,[String])]
fromGrammarHelper [] res = res
fromGrammarHelper (g:gm) res | any (\ (x, _) -> x == fst g) res = replaceG res (fst g) (snd g : snd (selectG res (fst g)))
                             | otherwise = fromGrammarHelper gm ((fst g, [snd g]):res)

selectG ::  [(Char,[String])] -> Char -> (Char, [String])
selectG xs ch = head (filter (\(x,_) -> x == ch) xs)

replaceG :: [(Char,[String])] -> Char -> [String] -> [(Char,[String])]
replaceG [] _ _ = []
replaceG ((x,g):xs) ch str = if x == ch then (ch, str) : xs else (x,g):replaceG xs ch str

testFst :: Predict -> [String] -> Bool
testFst pFst rls = not (any (/= "") ([inter (first pFst x) (first pFst y) | x <- rls, y <- rls, x /= y]))


testFollow :: Predict -> String -> [String] -> Bool
testFollow pFst nxt rls | "" `elem` rls = not (any (/="") ([inter (first pFst x) nxt | x <- rls, x/=""]))
                   | otherwise = True

-- Задача 7 ------------------------------------
buildFst :: Grammar -> Predict
buildFst g = sortBy sortPredicts (makeStep g (evalFst g (listOfTerm g)))

listOfTerm :: [(Char, String)] -> [(Char, String)]
listOfTerm xs = if null xs then [] else (fst (head xs), ""): listOfTerm (filter (\x -> fst x /= fst (head xs)) xs)

makeStep :: Grammar -> Predict -> Predict
makeStep gm pr = if stp1 == stp2 then stp1 else makeStep gm stp2
  where stp1 = foldl extendFst pr gm
        stp2 = foldl extendFst stp1 gm

evalFst :: Grammar -> Predict -> Predict
evalFst _ [] = []
evalFst gm (p:pFst) | (fst p, "") `elem` gm = (fst p, "$") : evalFst gm pFst
                    | otherwise = (fst p, "") : evalFst gm pFst

extendFst :: Predict -> Production -> Predict
extendFst pFst (_, []) = pFst
extendFst pFst (n, t:ts) | isUpper t && '$' `elem` snd fstY =
                            extendFst (replaceN pFst n (addWithout (snd fstA) (snd fstY))) (n,ts)
                         | isUpper t = replaceN pFst n (addAll (snd fstA) (snd fstY))
                         | otherwise = replaceN pFst n (addOne (snd fstA) t)
  where fstA = selectN pFst n
        fstY = selectN pFst t

selectN :: Predict -> Char -> (Char, String)
selectN xs ch = head (filter (\(x,_) -> x == ch) xs)

replaceN :: Predict -> Char -> String -> Predict
replaceN [] _ _ = []
replaceN ((x,g):xs) ch str = if x == ch then (ch, str) : xs else (x,g):replaceN xs ch str

-- Задача 8 ------------------------------------
buildNxt :: Grammar -> Predict -> Predict 
buildNxt g pFst = sortBy sortPredicts (makeNxtStep pFst (evalNxt g pFst (listOfTerm g)) (nontermTails g))

makeNxtStep :: Predict -> Predict -> Predict -> Predict
makeNxtStep pFst pNxt tl = if stp1 == stp2 then stp1 else makeNxtStep pFst stp2 tl
                where stp1 = makeNxtFull pFst pNxt tl
                      stp2 = makeNxtFull pFst stp1 tl

makeNxtFull :: Predict -> Predict -> Predict -> Predict
makeNxtFull _ pNxt [] = pNxt
makeNxtFull pFst pNxt (t:tl) = makeNxtFull pFst (extendNxtOne pFst (fst t) pNxt (snd t)) tl

nontermTails :: Grammar -> [(Char,String)]
nontermTails [] = []
nontermTails ((_,[]):gm) = nontermTails gm
nontermTails ((x,y:ys):gm) | isUpper y = (x,y:ys) : nontermTails ((x, ys):gm)
                             | otherwise = nontermTails ((x, ys):gm)

evalNxt :: Grammar -> Predict -> Predict -> Predict
evalNxt _ _ [] = []
evalNxt [] _ _ = []
evalNxt (g:gm) pr (t:terms) | fst g == fst t = (fst g,"$") : evalNxt gm pr terms
                        | otherwise = (fst t, "") : evalNxt gm pr terms


extendNxtOne :: Predict -> Char -> Predict -> String -> Predict
extendNxtOne _ _ _ [] = []
extendNxtOne pFst n pNxt (m:st) | '$' `elem` firstB = replaceN pNxt m (addAll nxtY (addWithout nxtA firstB))
                                | otherwise = replaceN pNxt m (addAll nxtY firstB)
      where firstB = first pFst st
            nxtY = tkPredict pNxt m
            nxtA = tkPredict pNxt n


---------------------Тестові дані ---------------------------
 
gr0, gr1, gr2, gr3, gr4, gr5:: Grammar
--  LL(1)-граматики
gr0 = [('S',"aAS"),('S',"b"), ('A',"a"), ('A',"bSA")]

gr1 = [('S',"TV"),('T',"d"),('T',"(S)"),('V',"+TV"),('V',"-TV"),('V',"")]

gr2 = [('E',"TU"),('U',""),('U',"+TU"),('U',"-TU"),
       ('T',"FV"),('V',""),('V',"*FV"),('V',"%FV"),('V',"/FV"),
       ('F',"d"),('F',"(E)")] 
-- не LL(1)-граматики
gr3 = [('S',"aAS"), ('S',"a"),('A',"SbA"),('A',"ba"),('S',"")]
gr4 = [('E',"E+T"),('E',"T"), ('T',"T*F"), ('T',"F"), ('F',"d"),('F',"(E)") ]   
gr5 = [('E',"E+T"), ('E',"E-T"),('E',"T"), 
       ('T',"T*F"), ('T',"T%F"), ('T',"T/F"), ('T',"F"), 
       ('F',"d"),('F',"(E)") ]

-- прогнозуючі таблиці початкових терміналів Fst
pFst0, pFst1, pFst2, pFst3, pFst4, pFst5 :: Predict
pFst0 = [('A',"ab"),('S',"ab")]

pFst1 = [('S',"(d"),('T',"(d"),('V',"$+-")]

pFst2 = [('E',"(d"),('F',"(d"),('T',"(d"),('U',"$+-"),('V',"$%*/")]

pFst3 = [('A',"ab"),('S',"$a")]

pFst4 = [('E',"(d"),('F',"(d"),('T',"(d")]

pFst5 = [('E',"(d"),('F',"(d"),('T',"(d")]

-- прогнозуючі таблиці наступних терміналів Nxt
pNxt0, pNxt1, pNxt2, pNxt3, pNxt4, pNxt5 :: Predict
pNxt0 = [('A',"ab"),('S',"$ab")]
pNxt1 = [('S',"$)"),('T',"$)+-"),('V',"$)")]
pNxt2 = [('E',"$)"),('F',"$%)*+-/"),('T',"$)+-"),('U',"$)"),('V',"$)+-")]
pNxt3 = [('A',"$ab"),('S',"$b")]
pNxt4 = [('E',"$)+"),('F',"$)*+"),('T',"$)*+")]
pNxt5 = [('E',"$)+-"),('F',"$%)*+-/"),('T',"$%)*+-/")]   

-- управляючі таблиці 
ctl0, ctl1, ctl2 :: Control 
ctl0 = [(('A','a'),2),(('A','b'),3),(('S','a'),0),(('S','b'),1)]

ctl1 = [(('S','('),0),(('S','d'),0),(('T','('),2),(('T','d'),1),
        (('V','$'),5),(('V',')'),5),(('V','+'),3),(('V','-'),4)]

ctl2 = [(('E','('),0),(('E','d'),0),(('F','('),10),(('F','d'),9),
        (('T','('),4),(('T','d'),4),(('U','$'),1),(('U',')'),1),
        (('U','+'),2),(('U','-'),3),(('V','$'),5),(('V','%'),7),
        (('V',')'),5),(('V','*'),6),(('V','+'),5),(('V','-'),5),(('V','/'),8)]

