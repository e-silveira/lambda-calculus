module Lambda where

import Data.List ((\\), elemIndex)

data Exp
  = Var Char
  | Abs Char Exp
  | App Exp Exp
  deriving (Eq)

instance Show Exp where
  show :: Exp -> String
  show (Var name) = [name]
  show (Abs name t) = "(λ " ++ [name] ++ " . " ++ show t ++ ")"
  show (App s t) = "(" ++ show s ++ " " ++ show t ++ ")"

freeVariables :: Exp -> [Char]
freeVariables (Var name) = [name]
freeVariables (Abs name t) = freeVariables t \\ [name]
freeVariables (App s t) = freeVariables s ++ freeVariables t

boundVariables :: Exp -> [Char]
boundVariables (Var _) = []
boundVariables (Abs name t) = name : boundVariables t
boundVariables (App s t) = boundVariables s ++ boundVariables t

substitute :: Char -> Exp -> Exp -> Exp
substitute name to from@(Var name') =
  if name == name'
    then to
    else from
substitute name to from@(Abs name' t)
  | name == name' = from
  | name' `notElem` fvTo = Abs name' $ substitute name to t
  | otherwise = substitute name to from'
  where
    fvTo = freeVariables to
    available = [x | x <- ['a' ..], x `notElem` fvTo ++ boundVariables from]
    from' = alphaConversion from available
substitute name to from@(App s t) = App s' t'
  where
    s' = substitute name to s
    t' = substitute name to t

alphaConversion :: Exp -> [Char] -> Exp
alphaConversion t@(Abs name _) available =
  alphaConversion' t name $ head available

alphaConversion' :: Exp -> Char -> Char -> Exp
alphaConversion' (Var name) from to =
  if name == from
    then Var to
    else Var name
alphaConversion' (Abs name t) from to =
  if name == from
    then Abs to t'
    else Abs name t'
  where
    t' = alphaConversion' t from to
alphaConversion' (App s t) from to = App s' t'
  where
    s' = alphaConversion' s from to
    t' = alphaConversion' t from to

isValue :: Exp -> Bool
isValue (Abs _ _) = True
isValue (Var _) = True
isValue _ = False

eval' :: Exp -> Exp
eval' (App (Abs x t12) t2) = if isValue t2
                                  then substitute x t2 t12
                                  else let t2' = eval' t2
                                       in App (Abs x t12) t2'
eval' (App t1 t2) = let t1' = eval' t1
                        in App t1' t2
eval' t = t

eval :: Exp -> Exp
eval t = if t == eval' t then t else eval (eval' t)