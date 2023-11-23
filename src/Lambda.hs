module Lambda where

import Data.List ((\\), elemIndex)

data LamExp
  = LamVar Char
  | LamAbs Char LamExp
  | LamApp LamExp LamExp
  deriving (Eq)

instance Show LamExp where
  show :: LamExp -> String
  show (LamVar name) = [name]
  show (LamAbs name t) = "(λ " ++ [name] ++ " . " ++ show t ++ ")"
  show (LamApp s t) = "(" ++ show s ++ " " ++ show t ++ ")"

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
isValue _ = False

eval' :: LamExp -> LamExp
eval' (LamApp (LamAbs x t12) t2) = if isValue t2
                                  then substitute x t2 t12
                                  else let t2' = eval' t2
                                       in LamApp (LamAbs x t12) t2'
eval' (LamApp t1 t2) = let t1' = eval' t1
                        in LamApp t1' t2
eval' t = t

eval :: LamExp -> LamExp
eval t = if t == eval' t then t else eval (eval' t)