
module Miedviediev10 where

import Data.List

-- ??????????? ???? ???? ????: ???????  ? ??????
--------------------------------------------------------------------
type Id    = String
data Value = I Int | A [(Int, Int)]  deriving (Eq, Show)
data Op    = Add | Minus | Mul | Less | Equal | Index  deriving (Eq, Show)

data Exp = Const Int
         | Var Id
         | OpApp Op Exp Exp
         | Cond Exp Exp Exp
         | FunApp Id [Exp]
         deriving (Eq, Show)

data Stmt = Assign Id Exp
          | AssignA Id Exp Exp
          | If Exp Stmt Stmt
          | While Exp Stmt
          | Call Id [Exp]
          | Block [VarDef] [Stmt]
          deriving (Eq, Show)

data VarDef  =  Arr Id | Int Id deriving (Eq, Show)

type FunDef  =  (Id, ([VarDef], Exp))
-- ??????? ?????????? ???? ???? ???????? ????, ?? ?????????????? ????????? ???? (?????!!)
type ProcDef = (Id, ([VarDef], Stmt))
type Program = ([VarDef], [FunDef], [ProcDef])

type StateP  = [(Id, Value)]  -- ???? ?????

data Type    = At | It  deriving (Eq, Show)
type FunEnv  = [(Id,[Type])]
type ProcEnv = [(Id,[Type])]
type VarEnv  = [(Id,Type)]

-- ?????? 1 ------------------------------------
updateValue :: Eq a => a -> b -> [(a,b)] -> [(a,b)]
updateValue a b (x@(a1, _):xs) | a == a1 = (a, b) : xs
                             | otherwise = x : updateValue a b xs
updateValue a b [] = [(a,b)]

-- ?????? 2 ------------------------------------
updateArray :: Value -> Value -> Value -> Value
updateArray (A a) (I i) (I v) = A (updateValue i v a)
updateArray a _ _ = a

-- ?????? 3 ------------------------------------
applyOp :: Op -> Value -> Value -> Value
applyOp Add (I a1) (I a2) = I (a1 + a2)
applyOp Minus (I a1) (I a2) = I (a1 - a2)
applyOp Mul (I a1) (I a2) = I (a1 * a2)
applyOp Less (I a1) (I a2) = I (fromEnum (a1 < a2))
applyOp Equal (I a1) (I a2) = I (fromEnum (a1 == a2))
applyOp Index (A as) (I a1) = case lookup a1 as of
                                 Just a -> I a
                                 Nothing -> I 0
applyOp _ _ _ = error "Operation not found!"

test3 :: Bool
test3 = all (== True)
           [applyOp Add (I 6) (I (- 2)) == I 4,
            applyOp Mul (I 3) (I 4) == I 12,
            applyOp Less (I 7) (I 0) == I 0,
            applyOp Equal (I 2) (I 2) == I 1,
            applyOp Index (A [(1, 1), (0, 3)]) (I 0) == I 3,
            applyOp Index (A [(1, 1), (0, 3)]) (I 2) == I 0]

-- ?????? 4 ------------------------------------
evExp ::  Exp -> [FunDef] -> StateP -> Value
evExp (Const x) _ _ = I x
evExp (Var y) _ st = findState st y
evExp (OpApp op a1 a2) fs st = applyOp op (evExp a1 fs st) (evExp a2 fs st)
-- a1 and a2 returns different?
evExp (Cond cond a1 a2) fs st = case evExp cond fs st of
                                  I 0 -> evExp a2 fs st
                                  _ -> evExp a1 fs st
evExp (FunApp fname as) fs st = evExp nexp fs nst
                    where (defs, nexp) = findFunc fs fname
                          nst = defsToStateP defs as fs st

defsToStateP :: [VarDef] -> [Exp] -> [FunDef] -> StateP -> StateP
defsToStateP vdefs exps fdefs st = zip names values
                                        where names = map (\x -> case x of
                                                                   Arr a -> a
                                                                   Int a -> a) vdefs
                                              values = evArgs exps fdefs st

findFunc :: [FunDef] -> Id -> ([VarDef], Exp)
findFunc ((a,b):xs) str | a == str = b
                        | otherwise = findFunc xs str
findFunc [] _ = error "findFunc not found!"

findState :: StateP -> Id -> Value
findState ((a,b):xs) str | a == str = b
                           | otherwise = findState xs str
findState [] _ = error "findState not found!"

findProc :: [ProcDef] -> Id -> ([VarDef], Stmt)
findProc ((a,b):xs) str | a == str = b
                        | otherwise = findProc xs str
findProc [] _ = error "findProc not found!"

evArgs :: [Exp] -> [FunDef] ->StateP -> [Value]
evArgs es defs st = map (\x -> evExp x defs st) es

test4 :: Bool
test4 = all (== True)
  [
  evExp (Const  1) [] sampleState == I 1,
  evExp (Var "y") [] sampleState == I 2,
  evExp (OpApp Add (Var "x") (Const 2)) [] sampleState == I 7,
  evExp (Cond (Const 1) (Var "x") (Const 9)) [] sampleState == I 5,
  evExp (FunApp "fib" [Const  6]) [fib] sampleState == I 8
  ]

-- ?????? 5 ------------------------------------
evStmt :: Stmt -> [FunDef] -> [ProcDef] -> StateP -> StateP
evStmt (Assign id1 exp1) dfx _ st = updateValue id1 (evExp exp1 dfx st) st
evStmt (AssignA id1 exp1 exp2) dfx _ st = updateValue id1 
  (updateArray (findState st id1) (evExp exp1 dfx st) (evExp exp2 dfx st)) st
evStmt (If exp1 s1 s2) dfx dpx st = case evExp exp1 dfx st of
                                      I 0 -> evStmt s2 dfx dpx st
                                      _ -> evStmt s1 dfx dpx st
evStmt (While exp1 s) dfx dpx st = until ((\e f t -> case evExp e f t of 
                                                      I 0 -> True
                                                      _ -> False) exp1 dfx)
                                                      (evStmt s dfx dpx) st
-- functions can have local variables
evStmt (Call id1 exps) dfx dpx st = removeLocals (evStmt s1 dfx dpx nst) names
        where nst = (zip names values) ++ st
              names = map (\x -> case x of
                               Arr a -> a
                               Int a -> a) vdefs
              values = evArgs exps dfx st
              (vdefs,s1) = findProc dpx id1 
              
                 
evStmt (Block vdefs ss) dfx dpx st = removeLocals nst (map (\x -> case x of
                                                                    Arr a -> a
                                                                    Int a -> a) vdefs)
        where nst = foldl (\st1 s -> evStmt s dfx dpx st1) (foldl (\st2 v -> (initv v):st2) st vdefs) ss


removeLocals :: StateP -> [Id] -> StateP
removeLocals st ids = [(y,z) | (y,z) <- st, y `notElem` ids]

test5 :: Bool
test5 = all (== True)
  [
  evStmt  sampleBlock  [] [sumA1] [("sA", I 0)] == [("sA", I 22)],
  evStmt  (Assign "y" (FunApp "sumA" [Var "a", Const 1])) [sumA] [] sampleState    ==  [("x", I 5), ("y", I 6), ("a", A [(2,3),(0,4),(1,2)])],
  evProgram pr2 == [("sA", I 22)],
  evProgram pr1 == [("gSum", I 15)]
  ]

-- ?????? 6 ------------------------------------
iswfExp :: Exp -> VarEnv -> FunEnv -> Maybe Type
iswfExp (Const _) _ _ = Just It
iswfExp (Var n) venv _ = findVarT venv n
iswfExp (OpApp op a1 a2) venv fenv = let i1 = iswfExp a1 venv fenv
                                         i2 = iswfExp a2 venv fenv
                                         in
                                      case (i1, i2) of
                                        (Just r1, Just r2) -> iswfOp op [r1, r2]
                                        _ -> Nothing
iswfExp (Cond cond a1 a2) venv fenv = let i1 = iswfExp cond venv fenv
                                          i2 = iswfExp a1 venv fenv
                                          i3 = iswfExp a2 venv fenv
                                          in
                                       case (i1, i2, i3) of
                                        (Just r1, Just r2, Just r3) -> iswfCond  [r1, r2, r3]
                                        _ -> Nothing
iswfExp (FunApp fname as) venv fenv = case findFunT fenv fname of
                                        Just r -> case cond && r == res of
                                                     True -> Just It
                                                     False -> Nothing
                                          where t = [iswfExp x venv fenv | x <- as]
                                                cond = all (\x -> True == case x of {Just _ -> True; Nothing -> False}) t
                                                res = map (\x -> case x of {Just i -> i; Nothing -> It}) t
                                        Nothing -> Nothing

findProcT :: ProcEnv -> Id -> Maybe [Type]
findProcT ((a,b):xs) str | a == str = Just b
                       | otherwise = findProcT xs str
findProcT [] _ = Nothing

findFunT :: FunEnv -> Id -> Maybe [Type]
findFunT ((a,b):xs) str | a == str = Just b
                       | otherwise = findFunT xs str
findFunT [] _ = Nothing

findVarT :: VarEnv -> Id -> Maybe Type
findVarT ((a,b):xs) str | a == str = Just b
                       | otherwise = findVarT xs str
findVarT [] _ = Nothing

test6 :: Bool
test6 = all (== True)
  [
  iswfExp (Var "a") varEnv  [] == Just At,
  iswfExp (Var "b") varEnv  [] == Nothing,
  iswfExp (FunApp "fib" [ Var "x"]) varEnv [("fib",[It])] ==  Just It,
  iswfExp (FunApp "fib" [Const 6, Var "x"]) varEnv [("fib",[It])] == Nothing

  ]

-- ?????? 7 ------------------------------------
iswfStmt :: Stmt -> VarEnv -> FunEnv -> ProcEnv -> Bool
iswfStmt (Assign id1 exp1) venv fenv penv = checkDuplic venv fenv penv && check
                    where check = case findVarT venv id1 of
                                    Just r1 -> res
                                      where res = case iswfExp exp1 venv fenv of
                                                    Just r2 -> r1 == r2
                                                    Nothing -> False
                                    Nothing -> False
iswfStmt (AssignA id1 exp1 exp2) venv fenv penv = checkDuplic venv fenv penv && check
                    where check = case (i1, i2, i3) of
                                    (Just r1, Just r2, Just r3) -> iswfAssignA [r1, r2, r3]
                                    _ -> False
                                  where i1 = findVarT venv id1
                                        i2 = iswfExp exp1 venv fenv
                                        i3 = iswfExp exp2 venv fenv
iswfStmt (If exp1 s1 s2) venv fenv penv = checkDuplic venv fenv penv && check
                                            && iswfStmt s1 venv fenv penv
                                            && iswfStmt s2 venv fenv penv
                    where check = case iswfExp exp1 venv fenv of
                                    Just It -> True
                                    _ -> False
iswfStmt (While exp1 s) venv fenv penv = checkDuplic venv fenv penv && check
                                            && iswfStmt s venv fenv penv
                    where check = case iswfExp exp1 venv fenv of
                                       Just It -> True
                                       _ -> False
iswfStmt (Call id1 exps) venv fenv penv = checkDuplic venv fenv penv && check
                    where check = case findProcT penv id1 of
                                    Just r -> cond && r == res
                                      where t = [iswfExp x venv fenv | x <- exps]
                                            cond = all (\x -> True == case x of {Just _ -> True; Nothing -> False}) t
                                            res = map (\x -> case x of {Just i -> i; Nothing -> It}) t
                                    Nothing -> False

iswfStmt (Block vdefs ss) venv fenv penv = checkDuplic venv fenv penv && check
                                              && length nenv == length (nub $ [fst x | x <- nenv])
                      where check = all (\x -> True == iswfStmt x (nenv ++ venv) fenv penv) ss
                            nenv = map (\x -> case x of {Arr r -> (r, At); Int r -> (r, It)}) vdefs

checkDuplic :: VarEnv -> FunEnv -> ProcEnv -> Bool
checkDuplic venv fenv penv = length venv == v && length fenv == f && length penv == p
                    where v = length $ nub $ [fst x | x <- venv]
                          f = length $ nub $ [fst x | x <- fenv]
                          p = length $ nub $ [fst x | x <- penv]

test7 :: Bool
test7 = all (== True)
  [
  iswfStmt  sampleBlock varEnv [] procEnv == True,
  iswfStmt  (Assign "y" (FunApp "sumA" [Var "a", Var "x"])) varEnv funEnv [] == True,
  iswfStmt  (Assign "y" (FunApp "sumA" [Var "x", Var "x"])) varEnv funEnv [] == False
  ]


-- ?????? 8 ------------------------------------
iswfFunDef :: FunDef -> FunEnv -> Bool
iswfFunDef (id1, (vdefs, exp1)) fenv = case findFunT fenv id1 of
                                          Just _ -> iexp
                                          _ -> False
    where iexp = case iswfExp exp1 nenv fenv of
                   Just It -> True
                   _ -> False
          nenv = map (\x -> case x of {Arr r -> (r, At); Int r -> (r, It)}) vdefs

iswfProcDef :: ProcDef -> VarEnv -> FunEnv -> ProcEnv -> Bool
iswfProcDef (_, (vdefs, s)) venv fenv penv = iswfStmt s (nenv ++ venv) fenv penv
    where nenv = map (\x -> case x of {Arr r -> (r, At); Int r -> (r, It)}) vdefs

test8 :: Bool
test8 = all (== True)
  [
  iswfFunDef fib funEnv == True,
  iswfProcDef sumA1 varEnv funEnv procEnv == False ,
  iswfProcDef sumA1 [("sA",At)] funEnv procEnv == False,
  iswfProcDef sumA1 [("sA",It)] [] []  == True
  ]


-- ?????? 9 ------------------------------------
iswfProgram :: Program -> Bool
iswfProgram (vdefs, fdefs, pdefs) = checkDuplic venv fenv penv 
                                    && check && main
                where main = any (\(id1, (x, _)) -> "main" == id1 && null x) pdefs
                      check = check1 && check2
                      check1 = all (\x -> iswfFunDef x fenv) fdefs
                      check2 = all (\x -> iswfProcDef x venv fenv penv) pdefs
                      venv = map (\x -> case x of {Arr r -> (r, At); Int r -> (r, It)}) vdefs
                      fenv = transformFDef fdefs
                      penv = transformPDef pdefs

transformFDef::[FunDef] -> FunEnv
transformFDef ((id1,(vdefs,_)):xs) = (id1,t):transformFDef xs
                where t = [case x of {Arr _ -> At;Int _-> It} | x<-vdefs]
transformFDef [] = []

transformPDef::[ProcDef] -> ProcEnv
transformPDef ((id1,(vdefs,_)):xs) = (id1,t):transformPDef xs
                where t = [case x of {Arr _ -> At;Int _-> It} | x<-vdefs]
transformPDef [] = []

test9 :: Bool
test9 = all (== True)
  [
  iswfProgram pr1 == True,
  iswfProgram pr2 == True
  ]


--- ????????? ??????? -----------------------------
lookUp :: Eq a => a -> [(a,b)] -> b
-- ??????????: ???? ? ?????? a ? ? ?????? ??? abx
lookUp a abx = maybe (error "lookUp") id (lookup a abx)

-- ?????? ????????? ???????? ???????
initv :: VarDef -> (Id, Value)
initv (Arr v) = (v, A [])
initv (Int v) = (v, I 0)

-- ?????????? ????????? ????????
evProgram :: Program -> StateP
evProgram (dvx, dfx, dpx) =
   let sb = map initv dvx
       ( _, s) = lookUp "main" dpx
   in  evStmt s dfx dpx sb

--  iswfOp o ts - ????????? ??????????? ????? ????????? ts
--     ???????? ???????? o ? ?????? ??? ?????????? Just t ??? Nothing
iswfOp :: Op -> [Type] -> Maybe Type
iswfOp Add   [It,It] = Just It
iswfOp Minus [It,It] = Just It
iswfOp Mul   [It,It] = Just It
iswfOp Less  [It,It] = Just It
iswfOp Equal [It,It] = Just It
iswfOp Index [At,It] = Just It
iswfOp _      _      = Nothing

--  iswfCond ts - ????????? ???????????  ????? ????????? ts
--     ???????? ?????? ? ?????? ??? ?????????? Just t ??? Nothing
iswfCond :: [Type] -> Maybe Type
iswfCond [It,It,It] = Just It
iswfCond [It,At,At] = Just At
iswfCond _          = Nothing

-- iswfAssignA ts ????????? ???????????  ????? ????????? ts
--   ???????? ???????????? ???????? ???????? ??????
iswfAssignA :: [Type] -> Bool
iswfAssignA [At,It,It] = True
iswfAssignA _          = False

---- ???? ??? ??????????  -----------------------
-- ???? ??? ??????????
sampleState :: StateP
sampleState = [("x",I 5),("y",I 2),("a", A [(2,3),(0,4), (1,2)])]

varEnv :: VarEnv
varEnv = [("x",It), ("y",It), ("a",At)]

-- ??????? ???????? ???? ?????
-- func biggest(m,n)= (m<n ? n : m)
biggest :: FunDef
biggest =("biggest",
          ([Int "m", Int "n"],
           Cond (OpApp  Less (Var "m") (Var "n"))  (Var "n")  (Var "m")
           )
         )
-- ???????, ?? ???????? ????? ?????????
-- func fib(n) = (n<3 ? 1 : fib(n-1) + fib(n-2))
fib :: FunDef
fib = ("fib",
       ([Int "n"],
        Cond (OpApp Less (Var "n") (Const 3))
             (Const 1)
             (OpApp Add (FunApp "fib" [OpApp Minus (Var "n") (Const 1)])
                        (FunApp "fib" [OpApp Minus (Var "n") (Const 2)]))
       )
      )

-- ??????? - ???? ????????? ?????? 0..n ...
-- func sumA(a[],n) = (n<0 ? 0 : a[n] + sumA (a,n-1))
sumA :: FunDef
sumA = ("sumA",
        ([Arr "a", Int "n"],
         Cond (OpApp Less (Var "n") (Const 0))
              (Const 0)
              (OpApp Add (OpApp Index (Var "a") (Var "n"))
                         (FunApp "sumA" [Var "a", OpApp Minus (Var "n")(Const 1)])
              )
        )
       )

funEnv :: FunEnv
funEnv = [("biggest",[It,It]),("fib", [It]),("sumA",[At,It])]

-- ??????? ????????? - ?????
sampleBlock :: Stmt
sampleBlock = Block [Arr "b"]
                 [AssignA "b" (Const 0) (Const 9), AssignA "b" (Const 2) (Const 5),
                  AssignA "b" (Const 3) (Const 7), AssignA "b" (Const 5) (Const 1),
                  Call "sumA1" [Var "b", Const 5]
                 ]

-- ????????? - ????????? ???? ?????...
-- proc gAdd(x,y) gSum = x + y
gAdd :: ProcDef
gAdd = ("gAdd",
        ([Int "x", Int "y"],
         Assign "gSum" (OpApp Add (Var "x") (Var "y"))
        )
       )

-- ????????? - ???? ????????? ?????? 0..n ...
-- proc sumA1(a[],n) {i;limit;
--      sA=0; i=0; limit=n+1;
--      while (i<limit){sA=sA+a[i]; i=i+1}
--                   }
sumA1 :: ProcDef
sumA1 = ("sumA1",
         ([Arr "a", Int "n"],
          Block [Int "i", Int "limit"]
            [Assign "sA" (Const 0), Assign "i" (Const 0),
             Assign "limit" (OpApp Add (Var "n") (Const 1)),
             While (OpApp Less (Var "i") (Var "limit"))
                   (Block []
                     [Assign "sA" (OpApp Add (Var "sA")
                                  (OpApp Index (Var "a") (Var "i"))),
                      Assign "i" (OpApp Add (Var "i") (Const 1))
                     ]
                   )
            ]
         )
        )

procEnv :: ProcEnv
procEnv = [("gAdd",[It,It]),("sumA1",[At,It])]

-- ????? ????????
-- gSum;
-- proc gAdd(x,y) gSum = x + y
-- proc main() call gAdd(5,10)
pr1 :: Program
pr1 = ([Int "gSum"], [], [gAdd, ("main",([],Call "gAdd" [Const  5, Const 10]))])

-- sA
-- proc sumA1(a[],n) {i;limit; .... }
-- proc main() {b[]; b[0]=9; b[2]=5; b[3]=7; b[5]=1;
--                   call sumA1 (b,5)
--             }

pr2 :: Program
pr2 = ([Int "sA"], [],
       [sumA1,
        ("main",([], sampleBlock))
       ])



