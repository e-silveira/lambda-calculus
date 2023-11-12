module Lambda where

import Data.List ((\\))

data Expression
  = Variable Char
  | Abstraction Char Expression
  | Application Expression Expression
  deriving (Show, Eq)

freeVariables :: Expression -> [Char]
freeVariables (Variable name) = [name]
freeVariables (Abstraction name t) = freeVariables t \\ [name]
freeVariables (Application s t) = freeVariables s ++ freeVariables t

boundVariables :: Expression -> [Char]
boundVariables (Variable _) = []
boundVariables (Abstraction name t) = name : boundVariables t
boundVariables (Application s t) = boundVariables s ++ boundVariables t

substitute :: Char -> Expression -> Expression -> Expression
substitute name to from@(Variable name') =
  if name == name'
    then to
    else from
substitute name to from@(Abstraction name' t)
  | name == name' = from
  | name' `notElem` fvTo = Abstraction name' $ substitute name to t
  | otherwise = substitute name to from'
  where
    fvTo = freeVariables to
    conflicting = [x | x <- ['a' ..], x `notElem` fvTo ++ boundVariables from]
    from' = alphaConversion from conflicting
substitute name to from@(Application s t) = Application s' t'
  where
    s' = substitute name to s
    t' = substitute name to t

alphaConversion :: Expression -> [Char] -> Expression
alphaConversion t@(Abstraction name _) available =
  alphaConversion' t name $ head available

alphaConversion' :: Expression -> Char -> Char -> Expression
alphaConversion' (Variable name) from to =
  if name == from
    then Variable to
    else Variable name
alphaConversion' (Abstraction name t) from to =
  if name == from
    then Abstraction to t'
    else Abstraction name t'
  where
    t' = alphaConversion' t from to
alphaConversion' (Application s t) from to = Application s' t'
  where
    s' = alphaConversion' s from to
    t' = alphaConversion' t from to

isValue :: Expression -> Bool
isValue (Abstraction _ _) = True
isValue _ = False

eval :: Expression -> Expression
eval (Application s t) = evalApplication s t
eval t = t

evalApplication :: Expression -> Expression -> Expression
evalApplication s@(Abstraction var s') t =
  if isValue t
    then substitute var t s'
    else Application s (eval t)
evalApplication s t = Application (eval s) t