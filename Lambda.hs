module Lambda where

data Expression = Variable Char
                | Abstraction Char Expression
                | Application Expression Expression deriving (Show, Eq)

freeVariables :: Expression -> [Char]
freeVariables (Variable x) = [x]
freeVariables (Abstraction x e) = [y | y <- freeVariables e, y /= x]
freeVariables (Application a b) = freeVariables a ++ freeVariables b

substitution :: Char -> Expression -> Expression -> Expression
substitution x s (Variable y) = if x == y then s else Variable y
substitution x s (Abstraction y t) = Abstraction y (if x /= y && y `notElem` freeVariables s then substitution x s t else t)
substitution x s (Application t q) = Application (substitution x s t) (substitution x s q)
