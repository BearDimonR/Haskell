module Miedviediev04 where

import Data.Char(isDigit, digitToInt)

-- Задача 1 -----------------------------------------
analyseG :: String -> Bool 
analyseG xs = maybe False null (s xs)

s :: String -> Maybe String
s ('a': xs) = 
  case s xs of
    Just ('b': xs2) -> 
      case a xs2 of
        Just ('a': xs3) -> Just xs3
        _ -> Nothing
    _ -> Nothing
s ('b': xs) = Just xs
s _ = Nothing

a :: String -> Maybe String
a ('b': 'a' : xs) = 
  case a xs of
    Just xs2 -> 
      case s xs2 of
        Just xs3 -> Just xs3
        _ -> Nothing
    _ -> Nothing 
a ('a' : xs) = Just xs
a _ = Nothing
   
-- Задача 2 ----------------------------------------
balance :: String -> Bool
balance xs = maybe False null (b xs)

b :: String -> Maybe String 
b xs = 
  case c xs of
    Just xs2 ->
      case e xs2 of
        Just xs3 -> Just xs3
        _ -> Nothing
    _ -> Nothing

c :: String -> Maybe String 
c (' ' : xs) = c xs
c xs = Just xs

e :: String -> Maybe String
e ('{' : xs) = 
  case b xs of
    Just ('}' : xs2) ->
      case b xs2 of
        Just xs3 -> Just xs3
        _ -> Nothing
    _ -> Nothing
e ('[' : xs) = 
  case b xs of
    Just (']' : xs2) ->
      case b xs2 of
        Just xs3 -> Just xs3
        _ -> Nothing
    _ -> Nothing
e ('(' : xs) = 
  case b xs of
    Just (')' : xs2) ->
      case b xs2 of
        Just xs3 -> Just xs3
        _ -> Nothing
    _ -> Nothing
e xs = Just xs

-- Задача 3 -----------------------------------------
analyseExpr :: String -> Bool 
analyseExpr xs = maybe False null (ae xs)

ae :: String -> Maybe String 
ae xs =
  case af xs of
      Just xs2 ->
        case aa xs2 of
          Just xs3 -> Just xs3
          _ -> Nothing
      _ -> Nothing

aa :: String -> Maybe String 
aa (d : xs) | d `elem` "+-*" =
  case af xs of
    Just xs2 ->
      case aa xs2 of
        Just xs3 -> Just xs3
        _ -> Nothing
    _ -> Nothing
aa xs = Just xs

af :: String -> Maybe String 
af ('(' : xs) = 
  case ae xs of
    Just (')' : xs2) -> Just xs2
    _ -> Nothing
af (d : xs) | isDigit d = Just xs
af _ = Nothing

-- Задача 4 -----------------------------------------
evalLeft :: String -> Maybe Int 
evalLeft xs = 
  case le xs of
    Just (v, xs2) | null xs2 -> Just v
    _ -> Nothing

le :: String -> Maybe (Int,String)
le xs =
  case lf xs of
           Just xs2 ->
             case la xs2 of
               Just xs3 -> Just xs3
               _ -> Nothing
           _ -> Nothing

la :: (Int,String) -> Maybe (Int,String) 
la (n, d : xs) | d `elem` "+-*" =
     case lf xs of
       Just (k,xs2) ->
         case la (inOp d n k, xs2) of
           Just (v, xs3) -> Just (v, xs3)
           _ -> Nothing
       _ -> Nothing
la xs = Just xs

lf :: String -> Maybe (Int,String)
lf ('(' : xs) = 
     case le xs of
       Just (n,')' : xs2) -> Just (n,xs2)
       _ -> Nothing
lf (d : xs) | isDigit d = Just (digitToInt d, xs)
lf _ = Nothing

-- Задача 5 -----------------------------------------
evalRigth :: String -> Maybe Int 
evalRigth xs =
  case re xs of
      Just (v, xs2) | null xs2 -> Just v
      _ -> Nothing

re :: String -> Maybe (Int,String) 
re xs = 
  case rf xs of
           Just xs2 ->
             case ra xs2 of
               Just xs3 -> Just xs3
               _ -> Nothing
           _ -> Nothing
           
ra :: (Int,String) -> Maybe (Int,String) 
ra (n, d : xs) | d `elem` "+-*" =
     case re xs of
       Just (k, xs2) -> Just(inOp d n k, xs2)
       _ -> Nothing
ra xs = Just xs

rf :: String -> Maybe (Int,String) 
rf ('(' : xs) = 
     case re xs of
       Just (n,')' : xs2) -> Just (n,xs2)
       _ -> Nothing
rf (d : xs) | isDigit d = Just (digitToInt d, xs)
rf _ = Nothing

-- Задача 6 -----------------------------------------
evalPrior :: String -> Maybe Int 
evalPrior xs =
  case pe xs of
        Just (v, xs2) | null xs2 -> Just v
        _ -> Nothing

pe :: String -> Maybe (Int,String) 
pe xs =
  case pt xs of
           Just xs2 ->
             case pa xs2 of
               Just xs3 -> Just xs3
               _ -> Nothing
           _ -> Nothing

pa :: (Int,String) -> Maybe (Int,String) 
pa (n, d : xs) | d `elem` "+-" =
     case pt xs of
       Just (k,xs2) ->
         case pa (inOp d n k, xs2) of
           Just (v, xs3) -> Just (v, xs3)
           _ -> Nothing
       _ -> Nothing
pa xs = Just xs

pt :: String -> Maybe (Int,String) 
pt xs =
  case pf xs of
           Just xs2 ->
             case pb xs2 of
               Just xs3 -> Just xs3
               _ -> Nothing
           _ -> Nothing

pb :: (Int,String) -> Maybe (Int,String) 
pb (n, d : xs) | d == '*' =
     case pf xs of
       Just (k,xs2) ->
         case pb (inOp d n k, xs2) of
           Just (v, xs3) -> Just (v, xs3)
           _ -> Nothing
       _ -> Nothing
pb xs = Just xs

pf :: String -> Maybe (Int,String)
pf ('(' : xs) = 
     case pe xs of
       Just (n,')' : xs2) -> Just (n,xs2)
       _ -> Nothing 
pf (d : xs) | isDigit d = Just (digitToInt d, xs)
pf _ = Nothing

------------------------------------------------------
match :: Char -> Maybe String -> Maybe String 
match c1 (Just (t:st)) | c1==t = Just st
match _ _                      = Nothing 

inOp :: Char -> Int -> Int -> Int
inOp c2 = case c2 of {'+' -> (+); '-' -> (-); '*' -> (*); _ -> undefined}