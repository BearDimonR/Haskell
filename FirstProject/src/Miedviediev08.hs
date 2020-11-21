module Miedviediev08 where

import Data.List
import GHC.Unicode
import Text.ParserCombinators.Parsec as Parsec


data Recur = Zero | Succ | Sel Int Int
           | Super Recur [Recur] 
           | Prim Recur Recur 
           | Mini Recur Int 
           | Name String  deriving (Show, Eq)
type System = [(String,Recur)]  

-- Задача 1 ------------------------------------
isNumbConst :: System -> Recur -> Bool 
isNumbConst sys r = case r of
                Zero -> True
                Super Succ f -> isNumbConst sys (head f)
                Name s -> if length xs == 1 then isNamesHelper sys s (snd(head xs)) && isNumbConst sys (snd(head xs)) else False
                    where xs = (filter (\(x, _) -> x == s) sys)
                _ -> False


test1 :: Bool
test1 =  False `notElem` [(isNumbConst x y) == z | (x,y,z)<- test1e]

test1e :: [(System, Recur, Bool)]
test1e = [
  ([], Zero, True),
  ([], Super Succ [Zero], True),
  ([], Succ, False),
  (syst1, (Name "const0"), True),
  ([], Super Succ [Super Succ [Zero]], True),
  (syst1, (Name "const2"), True)
  ]

-- Задача 2 ------------------------------------
evRank :: System -> Recur -> Int 
evRank sys r = case r of
           Zero -> 1
           Succ -> 1
           Sel n _ -> n
           Super _ al -> evRank sys (head al)
           Prim _ st -> (evRank sys st) - 1
           Mini b _ -> (evRank sys b) - 1
           Name f -> case (selectSys sys f) of
                        Just k -> evRank sys (snd(k))
                        Nothing -> 0


selectSys :: System -> String -> Maybe (String, Recur)
selectSys sys s | length res == 1 = Just (head res)
                | otherwise = Nothing
                  where res = filter (\(x, _) -> x == s) sys


-- Задача 3 ------------------------------------
isNames :: System -> Bool 
isNames sys = length (removeDuplicates sys) == length sys && False `notElem` [isNamesHelper sys x y | (x,y) <- sys]

removeDuplicates :: System -> System
removeDuplicates xs = if null xs then [] else head xs:removeDuplicates (filter (\(x,_) -> x /= fst(head xs)) xs)

isNamesHelper :: System -> String -> Recur -> Bool
isNamesHelper sys n f = case f of
                        Name s -> s /= n && s `elem` res
                        Zero -> True
                        Succ -> True
                        Sel _ _ -> True
                        Mini ff _ -> isNamesHelper sys n ff
                        Prim f1 f2 -> isNamesHelper sys n f1 && isNamesHelper sys n f2
                        Super ff fs -> isNamesHelper sys n ff && False `notElem` [isNamesHelper sys n x | x <- fs]
                        where res = case sysBefore sys n of
                                      Just s -> fst $ unzip s
                                      Nothing -> []



sysBefore :: System -> String -> Maybe System
sysBefore sys str = case elemIndex str (fst $ unzip sys) of
                       Just a -> Just (take a sys)
                       Nothing -> Nothing

-- Задача 4 ------------------------------------
isRecur :: System -> Recur -> Bool
isRecur sys f = case f of
                Name s -> case selectSys sys s of
                          Just a -> isNamesHelper sys s (snd a) && isRecur sys (snd a)
                          Nothing -> False
                Zero -> True
                Succ -> True
                Sel n k -> n >= k && n > 0 && k > 0
                Mini ff _ -> isRecur sys ff && ((evRank sys ff) > 1)
                Prim f1 f2 -> if isNumbConst sys f1 then 
                                isRecur sys f1 && isRecur sys f2 && evRank sys f1 == 1 && evRank sys f2 == 2
                                else isRecur sys f1 && isRecur sys f2 && ((evRank sys f2 - evRank sys f1) == 2)
   
                Super ff fs -> isRecur sys ff && False `notElem` [isRecur sys x | x <- fs]
                                  && rank == length fs && length fs > 0 && (False `notElem`(map (== head fss) (tail fss)))
                                where rank = evRank sys ff
                                      fss = map (evRank sys) fs


-- Задача 5 ------------------------------------
eval :: System -> Recur -> [Int] -> Int
eval sys r is = case r of
                Name s -> case (selectSys sys s) of
                          Just (_,a) -> eval sys a is
                          Nothing -> -1
                Zero -> 0
                Succ -> (head is) + 1
                Sel _ k -> is !! (k-1)
                Mini _ _ -> -1
                Prim f1 f2 -> last (until (\xs -> l <= xs !! len) (evalStep sys f2) (init is ++ [0] ++ [eval sys f1 (take (evRank sys f1) is)]))
                  where l = last is
                        len = length is - 1
                Super ff fs -> eval sys ff [(eval sys x is) | x <- fs]

evalStep :: System -> Recur -> [Int] -> [Int]
evalStep sys r is = free ++ [n + 1] ++ [eval sys r is]
        where free = init $ init is
              n = last $ init is

-- Задача 6 ------------------------------------
evalPart :: System -> Recur -> [Int] -> Maybe Int 
evalPart sys r is = case r of
                Name s -> case (selectSys sys s) of
                          Just (_,a) -> evalPart sys a is
                          Nothing -> Nothing
                Zero -> Just 0
                Succ -> Just ((head is) + 1)
                Sel _ k -> Just (is !! (k-1))
                Super ff fs -> case evalPartList sys fs is of
                               Just as -> evalPart sys ff as
                               Nothing -> Nothing
                Prim f1 f2 -> case (evalPart sys f1 (take (evRank sys f1) is)) of
                              Just a -> case (until (\xs -> case xs of
                                                              Just aa -> last is <= aa !! (length is - 1)
                                                              Nothing -> True)
                                                        (evalStepPart sys f2) (Just(init is ++ [0] ++ [a]))) of
                                          Just b -> Just (last b)
                                          Nothing -> Nothing
                              Nothing -> Nothing

                Mini f i -> case (until (\(xs, g) -> case xs of
                                                     Just a -> i <= last a || g <= 0
                                                     Nothing -> True)
                                                     (evalStepMin sys f) (Just (is++[-1]),1)) of
                            (Just xs, ii) -> if ii == 0 then Just (last xs) else Nothing
                            (Nothing, _) -> Nothing


evalPartList :: System -> [Recur] -> [Int] -> Maybe [Int]
evalPartList sys (r:rs) is = case evalPart sys r is of
                             Just a -> case (evalPartList sys rs is) of
                                       Just b -> Just (a : b)
                                       Nothing -> Nothing
                             Nothing -> Nothing
evalPartList _ [] _ = Just []

evalStepPart :: System -> Recur -> Maybe [Int] -> Maybe [Int]
evalStepPart sys r is = case is of
                          Just js -> case evalPart sys r js of
                                      Just a -> Just ((init $ init js) ++ [(last $ (init js)) + 1] ++ [a])
                                      Nothing -> Nothing
                          Nothing -> Nothing

evalStepMin :: System -> Recur -> (Maybe [Int],Int) -> (Maybe [Int],Int)
evalStepMin sys r (is,g) = case is of
                           Just js -> case evalPart sys r (init js++[last js + 1]) of
                                      Just a -> (Just (init js ++ [(last js) + 1]), a)
                                      Nothing -> (Nothing, g)
                           Nothing -> (Nothing, g)

-- Задача 7 ------------------------------------
parseRec :: String -> Maybe System 
parseRec str = case parse system "" (filter (\x -> not $ isSpace x) str) of
                    Right s -> Just s
                    Left _ -> Nothing

integer :: Parser Int
integer = do ds <- many1 digit
             return $ read ds

letter1 :: Parser Char
letter1 = oneOf "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

iden :: Parser String
iden = do l <- letter1
          rest <- many (letter1 <|> digit)
          return (l : rest)

recur :: Parser Recur
recur = do rec <- try base <|> try super <|> try prim <|> mini
           return rec

base :: Parser Recur
base = do b <- try zero <|> try succ1 <|> try sel <|> name1
          return b

zero :: Parser Recur
zero = do _ <- string "z1"
          return Zero

succ1 :: Parser Recur
succ1 = do _ <- string "a1"
           return Succ

sel :: Parser Recur
sel = do _ <- char 's'
         n <- digit
         k <- digit
         return (Sel (read [n]) (read [k]))

name1 :: Parser Recur
name1 =  do i <- iden
            return (Name i)

super :: Parser Recur
super = do _ <- char '('
           rc1 <- recur
           _ <- char ':'
           rc2 <- recur
           arr <- many ((char ',') >> recur)
           _ <- char ')'
           return (Super rc1 (rc2:arr))

prim :: Parser Recur
prim = do _ <- char '['
          rc1 <- recur
          _ <- char ','
          rc2 <- recur
          _ <- char ']'
          return (Prim rc1 rc2)

mini :: Parser Recur
mini = do _ <- char '{'
          rc1 <- recur
          _ <- char ','
          int <- integer
          _ <- char '}'
          return (Mini rc1 int)          

system :: Parser System
system = do a <- many (sst)
            _ <- eof
            return a
            
sst :: Parser (String, Recur)
sst = do i <- iden
         _ <- char '='
         rc <- recur
         _ <- char ';'
         return (i, rc)






---------------------Тестові дані -  -------
syst1, syst2 :: System
syst1 = [("const0", Zero)   
   , ("const0v2", Super Zero [Sel 2 1])
   , ("const0v3", Super Zero [Sel 3 1])
   , ("const1v2", Super Succ [Super Zero [Sel 2 1]])
   , ("const2", Super Succ [Super Succ [Zero]])
   , ("addition", Prim (Sel 1 1) (Super Succ [Sel 3 3 ]))
   , ("multiplication", Prim Zero (Super (Name "addition") [Sel 3 3, Sel 3 1]))  
   , ("notSignum", Prim (Super Succ [Zero]) (Super Zero [Sel 2 1]))
   , ("subtract1", Prim Zero (Sel 2 1))
   , ("subtraction", Prim (Sel 1 1) (Super (Name "subtract1") [Sel 3 3]))
   , ("subtractionRev", Super (Name "subtraction") [Sel 2 2, Sel 2 1])     
   , ("subtractionAbs", Super (Name "addition") [Name "subtraction", Name "subtractionRev"])  
   , ("subtractionAbs3", Super (Name "subtractionAbs") [Sel 3 1, Super (Name "addition") [Sel 3 2, Sel 3 3]])  
   , ("subtractionPart", Mini (Name "subtractionAbs3") 100)    
   ]

syst2 = [("f1", Super Succ [Zero])
        ,("f2", Super Succ [Name "f2"])
        ]


sysStr1,sysStr2 :: String    
sysStr1 = " const0 = z1; const0v2  = (z1 : s21); const0v3 = (z1:s31);\n\
          \  const1v2 = (a1 : (z1 : s21));  \n\
          \  const2= (a1:(a1:z1)); addition = [s11, (a1:s33)] ;\n\
          \  multiplication = [z1 , (addition: s33,s31)]; \n\
	  \  notSignum = [(a1:z1),(z1:s21)];\n\
	  \  subtract1 = [z1,s21]; subtraction = [s11, (subtract1:s33)];\n\
	  \  subtractionRev = (subtraction : s22, s21);\n\
          \  subtractionAbs = (addition: subtraction, subtractionRev); \n\
          \  subtractionAbs3=(subtractionAbs:s31, (addition:s32,s33))  ;\n \
          \ subtractionPart = {subtractionAbs3, 100 };"
 
sysStr2 = " f1 = (a1:z1); f2 = (a1, f2);"
