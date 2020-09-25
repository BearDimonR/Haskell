module Miedviediev03 where

type Algorithm    = [Substitution]
type Substitution = (String,String,Bool)
type ConfigA      = (Bool, Int, String)

data Command = Z Int | S Int | T Int Int | J Int Int Int deriving (Show, Eq)
type Program = [Command]
type ConfigC = (Int, Int, [Int])

-- + Задача 1 ------------------------------------
isPrefix :: String -> String -> Bool
isPrefix bs xs = bs == take (length bs) xs

-- + Задача 2 ------------------------------------
substitute :: Substitution -> Int -> String -> String
substitute (ls, rs, _) i str = take i str ++ rs ++ drop (i + length ls) str

-- + Задача 3------------------------------------
findPosition :: String -> Substitution -> [(Substitution,Int)] 
findPosition str (ls, rs, p) = [((ls, rs, p), i) | i <- [0..length str], isPrefix ls (drop i str)]

-- + Задача 4 ------------------------------------
findAll :: Algorithm -> String -> [(Substitution,Int)]  
findAll algo str
  | null algo = []
  | otherwise = findPosition str (head algo) ++ findAll (tail algo) str

-- + Задача 5 ------------------------------------
stepA :: Algorithm -> ConfigA -> ConfigA
stepA algo (p, i, str)
  | not p = error "Can't make next step!"
  | otherwise = (not pr, i+1, substitute (ls, rs, pr) k str)
    where ((ls, rs, pr), k) = head (findAll algo str)

-- + Задача 6 ------------------------------------
evalA :: Algorithm -> Int -> String -> Maybe String 
evalA algo n str = evalAHelper algo n str 0
  
evalAHelper :: Algorithm -> Int -> String -> Int -> Maybe String
evalAHelper algo n str i
  | n == i = Nothing
  | not p = Just w
  | otherwise = evalAHelper algo n w k
  where (p, k, w) = stepA algo (True, i, str)

-- + Задача 7 ------------------------------------
maximReg :: Program -> Int
maximReg pr
  | null pr = 1
  | otherwise = max (checkReg(head pr)) (maximReg (tail pr))

checkReg :: Command -> Int
checkReg (Z x)= x
checkReg (S x) = x
checkReg (T x y) = max x y
checkReg (J x y _) = max x y


-- + Задача 8 ------------------------------------
ini :: Program -> [Int] -> [Int] 
ini pr xs
 | initL >= regL = xs
 | otherwise = xs ++ replicate (regL - initL) 0
 where regL = maximReg pr
       initL = length xs

upd :: [Int] -> Int -> Int-> [Int]
upd reg i n = if length reg <= i then error "Index out of bounds!" else take i reg ++ n : drop (i+1) reg

-- + Задача 9 ------------------------------------
stepC :: Program -> ConfigC -> ConfigC
stepC pr (nm, st, reg)
  | nm > length pr = error "Can't make next step!"
  | otherwise =
    case command of
      (Z x) -> (nm + 1, st + 1, upd reg (x-1) 0)
      (S x) -> (nm + 1, st + 1, upd reg (x-1) (1 + (reg !! max 0 (x-1))))
      (T x y) ->(nm + 1, st + 1, upd (upd reg (y - 1) (reg !! max 0 (x - 1))) (x - 1) (reg !! max 0 (y - 1)))
      (J x y z) -> if (reg !! max 0 (x - 1)) == (reg !! max 0 (y - 1)) then (z, st + 1, reg) else (nm + 1, st + 1, reg)
    where command = pr !! max 0 (nm - 1)

-- + Задача 10 ------------------------------------
evalC :: Program -> Int -> [Int] -> Maybe Int 
evalC pr n reg = evalCHelper pr 0 (ini pr reg) 1 n

evalCHelper :: Program -> Int -> [Int] -> Int -> Int -> Maybe Int
evalCHelper pr n reg i m
  | n == m = Nothing
  | nm > length pr = Just (head regN)
  | otherwise = evalCHelper pr st regN nm m
  where (nm, st, regN) = stepC pr (i, n, reg)

---------------------Тестові дані - Нормальні алгоритми Маркова ---------------------------
clearBeginOne, addEnd, reverse, multiply:: Algorithm 
-- стирає перший символ вхідного слова (алфавіт {a,b})
clearBeginOne = [ ("ca", "", True)
                , ("cb", "", True)
                , ("", "c", False)
                ] 

-- дописує abb в кінець вхідного слова (алфавіт {a,b})
addEnd = [ ("ca", "ac", False)
         , ("cb", "bc", False)
         , ("c", "abb", True)
         , ("", "c", False)
         ] 
-- зеркальне відображення вхідного слова (алфавіт {a,b})
reverse = [ ("cc", "d", False)
          , ("dc", "d", False)
          , ("da", "ad", False) 
          , ("db", "bd", False) 
          , ("d", "", True) 
          , ("caa", "aca", False) 
          , ("cab", "bca", False) 
          , ("cba", "acb", False)
          , ("cbb", "bcb", False) 
          , ("", "c", False) 
          ]

-- добуток натуральних чисел 
--  multiply ("|||#||") = "||||||"  3*2 = 6
multiply = [("a|", "|ba", False)
            ,("a", "", False)
            ,("b|", "|b", False)
            ,("|#", "#a", False)
            ,("#", "c", False)
            ,("c|", "c", False)
            ,("cb", "|c", False)
            ,("c", "", True)
            ]

---------------------Тестові дані - Програми МНР ---------------------------
notSignum, addition, subtraction :: Program 
-- функція notSignum x
notSignum = [Z 2, J 1 2 5, Z 1, J 1 1 6, S 1] 

-- функція додавання  addition x y = x+y
addition = [Z 3, J 3 2 6, S 1, S 3, J 1 1 2]

-- функція віднімання subtraction x y = x-y, визначена для x>=y 
subtraction = [Z 3, J 1 2 6, S 2, S 3, J 1 1 2, T 3 1]
