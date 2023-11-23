module Lambda where

import           Data.List  (elemIndex, (\\))
import           Expression

data Exp
  = Var Char
  | Abs Char Exp
  | App Exp Exp
  deriving (Eq)

instance Show Exp where
  show :: Exp -> String
  show (Var name)   = [name]
  show (Abs name t) = "(λ " ++ [name] ++ " . " ++ show t ++ ")"
  show (App s t)    = "(" ++ show s ++ " " ++ show t ++ ")"

instance Expression Exp where
  evalStep :: Exp -> Exp
  evalStep _ = error "This function should not be called."

{--
  evalStep :: Exp -> Exp
  evalStep (App (Abs x body) arg)
    | isValue arg = subs x arg body
    | otherwise   = App (Abs x body) arg'
    where arg' = evalStep arg
  evalStep (App t1 t2) = let t1' = evalStep t1 in App t1' t2
  evalStep t = t
--}

freeVariables :: Exp -> [Char]
freeVariables (Var name)   = [name]
freeVariables (Abs name t) = freeVariables t \\ [name]
freeVariables (App s t)    = freeVariables s ++ freeVariables t

{--
boundVariables :: Exp -> [Char]
boundVariables (Var _) = []
boundVariables (Abs name t) = name : boundVariables t
boundVariables (App s t) = boundVariables s ++ boundVariables t

subs :: Char -> Exp -> Exp -> Exp
subs name to from@(Var name') =
  if name == name'
    then to
    else from
subs name to from@(Abs name' t)
  | name == name' = from
  | name' `notElem` fvTo = Abs name' $ subs name to t
  | otherwise = subs name to from'
  where
    fvTo = freeVariables to
    available = [x | x <- ['a' ..], x `notElem` fvTo ++ boundVariables from]
    from' = alphaConversion from available
subs name to from@(App s t) = App s' t'
  where
    s' = subs name to s
    t' = subs name to t

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
--}
