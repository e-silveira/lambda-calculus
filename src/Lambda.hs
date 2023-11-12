module Lambda where

import Data.List ((\\))

data LamExp
  = LamVar Char
  | LamAbs Char LamExp
  | LamApp LamExp LamExp
  deriving (Show, Eq)

freeVariables :: LamExp -> [Char]
freeVariables (LamVar name) = [name]
freeVariables (LamAbs name t) = freeVariables t \\ [name]
freeVariables (LamApp s t) = freeVariables s ++ freeVariables t

boundVariables :: LamExp -> [Char]
boundVariables (LamVar _) = []
boundVariables (LamAbs name t) = name : boundVariables t
boundVariables (LamApp s t) = boundVariables s ++ boundVariables t

substitute :: Char -> LamExp -> LamExp -> LamExp
substitute name to from@(LamVar name') =
  if name == name'
    then to
    else from
substitute name to from@(LamAbs name' t)
  | name == name' = from
  | name' `notElem` fvTo = LamAbs name' $ substitute name to t
  | otherwise = substitute name to from'
  where
    fvTo = freeVariables to
    available = [x | x <- ['a' ..], x `notElem` fvTo ++ boundVariables from]
    from' = alphaConversion from available
substitute name to from@(LamApp s t) = LamApp s' t'
  where
    s' = substitute name to s
    t' = substitute name to t

alphaConversion :: LamExp -> [Char] -> LamExp
alphaConversion t@(LamAbs name _) available =
  alphaConversion' t name $ head available

alphaConversion' :: LamExp -> Char -> Char -> LamExp
alphaConversion' (LamVar name) from to =
  if name == from
    then LamVar to
    else LamVar name
alphaConversion' (LamAbs name t) from to =
  if name == from
    then LamAbs to t'
    else LamAbs name t'
  where
    t' = alphaConversion' t from to
alphaConversion' (LamApp s t) from to = LamApp s' t'
  where
    s' = alphaConversion' s from to
    t' = alphaConversion' t from to

isValue :: LamExp -> Bool
isValue (LamAbs _ _) = True
isValue (LamVar _) = True
isValue (LamApp _ _) = False

eval :: LamExp -> LamExp
eval (LamApp s t) = evalLamApp s t
eval t = t

evalLamApp :: LamExp -> LamExp -> LamExp
evalLamApp s@(LamAbs var s') t =
  if isValue t
    then substitute var t s'
    else LamApp s (eval t)
evalLamApp s t = LamApp (eval s) t

toString :: LamExp -> String
toString (LamVar name) = [name]
toString (LamAbs name t) = "(lambda " ++ [name] ++ " . " ++ toString t ++ ")"
toString (LamApp s t) = "(" ++ toString s ++ " " ++ toString t ++ ")"