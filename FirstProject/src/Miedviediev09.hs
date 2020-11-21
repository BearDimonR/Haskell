module Miedviediev09 where

import Data.List
import qualified Text.ParserCombinators.Parsec as P

data RE = Null   |
          Term Char |
          Seq RE RE |
          Alt RE RE |
          Rep RE    |
          Plus RE   |
          Opt RE
        deriving (Eq, Show)

type State = Int
data Label = C Char | Eps deriving (Eq, Ord, Show)
type Transition = (State, State, Label)
type Automation = (State, [State], [Transition])

type MetaState = [State]
type MetaTransition = (MetaState, MetaState, Label)

-- Задача 1 -----------------------------------------
simplify :: RE -> RE   
simplify (Opt a) = Alt (simplify a) Null
simplify (Plus a) = Seq (simplify a) (Rep (simplify a))
simplify (Term a) = Term a
simplify (Seq a b) = Seq (simplify a) (simplify b)
simplify (Alt a b) = Alt (simplify a) (simplify b)
simplify (Rep a) = Rep (simplify a)
simplify Null = Null

-- Задача 2 -----------------------------------------
isTerminal :: Automation -> State -> Bool 
isTerminal (_, ss, _) s = s `elem` ss

isEssential :: Automation -> State -> Bool 
isEssential  auto@(_, _, ts) s = isTerminal auto s || any (\(x, _, y) -> x == s && check y) ts

check :: Label -> Bool
check Eps = False
check (C _) = True

-- Задача 3 -----------------------------------------
transitionsFrom :: Automation -> State -> [Transition]
transitionsFrom (_, _, ts) s = filter (\(x, _, _) -> s == x) ts

-- Задача 4 -----------------------------------------
labels :: [Transition] -> [Label]
labels ts = filter check (nub (map (\(_,_,l) -> l) ts))

-- Задача 5 -----------------------------------------
acceptsDA :: Automation -> String -> Bool
acceptsDA auto@(sts, _, _) str = null r_str && isTerminal auto r_s && r_b
  where (r_b, r_s, r_str) = until (\(x, _, z) -> null z || not x) (findNextDa auto) (True, sts, str)

findNextDa :: Automation -> (Bool, State, String) -> (Bool, State, String)
findNextDa auto (_, s, str) = case nextState (transitionsFrom auto s) (head str) of
                                Just st -> (True, st, tail str)
                                Nothing -> (False, s, str)

nextState :: [Transition] -> Char -> Maybe State
nextState ((_, st, C c):ts) s | s == c = Just st
                                | otherwise = nextState ts s
nextState _ _ = Nothing

-- Задача 6 -----------------------------------------
stStep  :: Automation -> State -> Label -> [State]
setStep :: Automation -> [State] -> Label -> [State]
closure :: Automation -> [State] -> [State]

stStep auto st l = sort [y | (_, y, z) <- transitionsFrom auto st, z == l]


-- test wrong results
setStep auto (st:sts) l =  sort $ stStep auto st l ++ setStep auto sts l
setStep _ [] _ = []

closure auto sts = snd (until fst (closureNext auto) (False, sort sts))

closureNext :: Automation -> (Bool, [State]) -> (Bool, [State])
closureNext auto (_, sts) | res == sts = (True, sts)
                          | otherwise = (False, res)
                    where res = sort $ nub $ sts ++ setStep auto sts Eps

-- Задача 7 -----------------------------------------
accepts :: Automation -> String -> Bool
accepts auto@(st, _, _) = acceptsStep auto st

acceptsStep :: Automation -> State -> String -> Bool
acceptsStep auto st (ch:str) = or [acceptsStep auto y str | y <- setStep auto (closure auto [st]) (C ch)]
acceptsStep (_, sts, _) st [] = st `elem` sts

-- Задача 8 -----------------------------------------
makeNDA :: RE -> Automation
makeNDA re = (1, [2], sort transitionsAll)
  where (transitionsAll, _) = make (simplify re) 1 2 3

make :: RE -> Int -> Int -> Int -> ([Transition], Int) 
make r st end n = case r of
                    Null -> ([(st, end, Eps)], n)
                    Term c -> ([(st, end, C c)], n)
                    Rep a -> ([(st, end, Eps), (st, n, Eps), (n + 1, end, Eps), (n + 1, n, Eps)] ++ res1, n1)
                                          where (res1, n1) = make a n (n + 1) (n + 2)
                    Seq a b -> ([(n, n+1, Eps)] ++ res1 ++ res2, n2)
                      where (res1, n1) = make a st n (n + 2)
                            (res2, n2) = make b (n + 1) end n1
                    Alt a b -> ([(st, n, Eps), (st, n + 2, Eps),
                                 (n + 1, end, Eps), (n + 3, end, Eps)] ++ res2 ++ res1, n2)
                      where (res1, n1) = make a n (n + 1) (n + 4)
                            (res2, n2) = make b (n + 2) (n + 3) n1
                    _ -> error "make simplify error"

-- Задача 9 -----------------------------------------
parseReg :: String -> Maybe RE 
parseReg str = case P.parse reg "" str of
                Right a -> Just a
                Left _ -> Nothing

reg :: P.Parser RE
reg = do r <- rexpr
         _ <- P.eof
         return r

rexpr :: P.Parser RE
rexpr = do t <- rterm
           r_or <- P.many regor
           return (if null r_or then t else Alt t (makeAlt r_or))

makeAlt :: [RE] -> RE
makeAlt [x] = x
makeAlt (x:xs) = Alt x (makeAlt xs)
makeAlt [] = error "makeAlt empty!"

regor :: P.Parser RE
regor = do _ <- P.char '|'
           rterm

rterm :: P.Parser RE
rterm = do f <- rfact
           fs <- P.many rfact
           return (if null fs then f else Seq f (makeSeq fs))

makeSeq :: [RE] -> RE
makeSeq [x] = x
makeSeq (x:xs) = Seq x (makeSeq xs)
makeSeq [] = error "makeSeq empty!"

rfact :: P.Parser RE
rfact = do p <- prime
           op <- P.many (P.oneOf "+*?")
           return (makeOp p op)

makeOp :: RE  -> String -> RE
makeOp r (x:xs) = case x of
                    '?' -> makeOp (Opt r) xs
                    '*' -> makeOp (Rep r) xs
                    '+' -> makeOp (Plus r) xs
                    _ -> Null
makeOp r [] = r

prime :: P.Parser RE
prime = do P.try rsymb P.<|> prntl

rsymb :: P.Parser RE
rsymb = do ch <- P.noneOf "*+|()?"
           return (Term ch)

prntl :: P.Parser RE
prntl = do _ <- P.char '('
           r <- rexpr
           _ <- P.char ')'
           return r

-- Задача 10 -----------------------------------------
makeDA' :: Automation -> (MetaState, [MetaState], [MetaTransition])
makeDA' auto@(st, _, _) = (head gmsx, bmsx, nub mtrx)
    where (gmsx, bmsx, mtrx) = until (\(_, y, _) -> null y) (makeDA'Step auto) 
              ([], [filter (isEssential auto) (closure auto [st])], [])

makeDA'Step :: Automation -> ([MetaState], [MetaState], [MetaTransition]) -> ([MetaState], [MetaState], [MetaTransition])
makeDA'Step auto (gmsx, allBmsx@(msx:bmsx), mtrx) = (gmsx++[msx], bmsx ++ uniq, mtrx ++ trs)
      where lbls = labels (concatMap (transitionsFrom auto) msx)
            sts = buildSts auto msx lbls
            trs = makeTrans msx sts lbls
            uniq = filter (\x -> x `notElem` allBmsx && x `notElem` gmsx) sts      
makeDA'Step _ (_, [], _) = error "makeDA'Step empty bmsx!"

makeTrans :: MetaState -> [MetaState] -> [Label] -> [MetaTransition]
makeTrans msx sts lbls = [(msx, sts!!x, lbls!!x) | x <- [0..length lbls - 1]]

buildSts :: Automation -> MetaState -> [Label] -> [MetaState]
buildSts auto st (l:lbls) = filter (isEssential auto) 
          (nub $ next ++ closure auto next) : buildSts auto st lbls
          where next = setStep auto st l
buildSts _ _ [] = []


makeDA :: Automation -> Automation
makeDA auto  = (1, end, trs)
    where (gmsx, _, mtrx) = makeDA' auto
          (end, trs) = makeNormal auto gmsx mtrx
          
makeNormal :: Automation -> MetaState -> [MetaTransition] -> ([State], [Transition])
makeNormal auto gmsx mtrx = (\(_, y, z) -> (y, z)) (foldl (makeNormalHelper auto) ([(gmsx, 1)], [], []) mtrx)

makeNormalHelper :: Automation -> ([(MetaState, Int)], [Int], [Transition]) -> MetaTransition -> ([(MetaState, Int)], [Int], [Transition]) 
makeNormalHelper auto@(_, end, _) (st, n, tr) (mst1, mst2, lbl) 
      | any (`elem` end) (closure auto mst2) = 
              (nnst2, sort $ nub $ n ++ [nst2], sort $ nub $ tr ++ [(nst1, nst2, lbl)])
      | otherwise = (nnst2, sort n, sort $ tr ++ [(nst1, nst2, lbl)])                                                          
      where nst1 = calcN st mst1
            nnst1 = nub $ st ++ [(mst1, nst1)]
            nst2 = calcN nnst1 mst2
            nnst2 = nub $ nnst1 ++ [(mst2, nst2)]
           
calcN :: [(MetaState, Int)] -> MetaState -> Int
calcN mst st | null res = snd (last mst) + 1
             | otherwise = snd $ head res
      where res = filter (\x -> fst x == st) mst 

-------------------------------------------------------
-- showRE - Функція може бути корисною при тестуванні
showRE :: RE -> String
showRE (Seq re re') = showRE re ++ showRE re'
showRE (Alt re re') = "(" ++ showRE re ++ "|" ++ showRE re' ++ ")"
showRE (Rep re)     = showRE' re ++ "*"
showRE (Plus re)    = showRE' re ++ "+"
showRE (Opt re)     =  showRE' re ++ "?"
showRE re           = showRE' re

showRE' :: RE -> String
showRE' Null      = ""
showRE' (Term c)  = [c]
showRE' (Alt re re') = showRE (Alt re re')
showRE' re        = "(" ++ showRE re ++ ")"

--------------------------------------------------------
-- Тестові приклади
reFigureS, re1S, re2S, re3S, re4S, re5S, re6S :: String
reFigureS = "(a|b)*c"
re1S = "(x|y)(1|2)"
re2S = "x'*"
re3S = "(ab|c)*"
re4S = "(a?)a"
re5S = "(ab)?d+"
re6S = "c?*"

reFigure, re1, re2, re3, re4, re5, re6 :: RE
reFigure = Seq (Rep (Alt (Term 'a') (Term 'b'))) (Term 'c')
re1 = Seq (Alt (Term 'x') (Term 'y')) (Alt (Term '1') (Term '2'))
re2 = Seq (Term 'x') (Rep (Term '\''))
re3 = Rep (Alt (Seq (Term 'a') (Term 'b')) (Term 'c'))
re4 = Seq (Opt(Term 'a')) (Term 'a')
re5 = Seq (Opt (Seq (Term 'a') (Term 'b'))) (Plus (Term 'd'))
re6 = Rep (Opt (Term 'c'))

ndaFigure, nda1, nda2, nda3, nda4, nda5, nda6, ndaTest :: Automation
daFigure, da1, da2, da3, da4, da5, da6 :: Automation
ndaFigure
  = (1,[2],[(1,3,Eps),(1,5,Eps),(3,4,Eps),(4,2,C 'c'),(5,7,Eps),
            (5,9,Eps),(6,3,Eps),(6,5,Eps),(7,8,C 'a'),(8,6,Eps),
            (9,10,C 'b'),(10,6,Eps)])
daFigure
  = (1,[2],[(1,1,C 'a'),(1,1,C 'b'),(1,2,C 'c')])

nda1 = (1,[2],[(1,5,Eps),(1,7,Eps),(3,4,Eps),(4,9,Eps),(4,11,Eps),
            (5,6,C 'x'),(6,3,Eps),(7,8,C 'y'),(8,3,Eps),(9,10,C '1'),
            (10,2,Eps),(11,12,C '2'),(12,2,Eps)])
da1 = (1,[3],
     [(1,2,C 'x'),(1,2,C 'y'),(2,3,C '1'),(2,3,C '2')])

nda2 = (1,[2],[(1,3,C 'x'),(3,4,Eps),(4,2,Eps),(4,5,Eps),(5,6,C '\''),
            (6,2,Eps),(6,5,Eps)])
da2 = (1,[2],
     [(1,2,C 'x'),(2,2,C '\'')])

nda3 = (1,[2],[(1,2,Eps),(1,3,Eps),(3,5,Eps),(3,7,Eps),(4,2,Eps),
            (4,3,Eps), (5,9,C 'a'),(6,4,Eps),(7,8,C 'c'),(8,4,Eps),
            (9,10,Eps),(10,6,C 'b')])
da3 = (1,[1],
     [(1,1,C 'c'),(1,2,C 'a'),(2,1,C 'b')])

nda4 = (1,[2],[(1,5,Eps),(1,7,Eps),(3,4,Eps),(4,2,C 'a'),(5,6,C 'a'),
            (6,3,Eps),(7,8,Eps),(8,3,Eps)])
da4 = (1,[2,3],[(1,2,C 'a'),(2,3,C 'a')])

nda5 = (1,[2],[(1,5,Eps),(1,7,Eps),(3,4,Eps),(4,11,C 'd'),(5,9,C 'a'),
            (6,3,Eps),(7,8,Eps),(8,3,Eps),(9,10,Eps),(10,6,C 'b'),
            (11,12,Eps),(12,2,Eps),(12,13,Eps),(13,14,C 'd'),
            (14,2,Eps),(14,13,Eps)])
da5 = (1,[2],[(1,2,C 'd'),(1,3,C 'a'),(2,2,C 'd'),(3,4,C 'b'),
            (4,2,C 'd')])

nda6 = (1,[2], [(1,2,Eps),(1,3,Eps),(3,5,Eps),(5,6, C 'c'), (6,4,Eps), 
                (4,2,Eps), (3,7,Eps), (7,8,Eps), (8,4,Eps), (4,3,Eps)]) 
da6 = (1,[1], [(1,1, C 'c')])

ndaTest = (1, [1], [(1,2, C 'a'), (1,4, Eps), (1,3, C 'b'), (2,3, Eps),
              (3,5, Eps), (3,4, C 'a'), (4,4, Eps), (4,1, Eps), (5,2, Eps), (5,4,Eps)] )
