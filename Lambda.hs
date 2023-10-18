module Lambda where

data Expression = Variable Char
                | Abstraction Char Expression
                | Application Expression Expression deriving (Show, Eq)

freeVariables :: Expression -> [Char]
freeVariables (Variable x) = [x]
freeVariables (Abstraction x e) = [y | y <- freeVariables e, y /= x]
freeVariables (Application a b) = freeVariables a ++ freeVariables b
