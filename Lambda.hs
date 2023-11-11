module Lambda where

import Data.List ( (\\) )
import Distribution.Backpack.ConfiguredComponent (toConfiguredComponent)
import Distribution.Simple.Setup (testOptions')

data Expression = Variable Char
                | Abstraction Char Expression
                | Application Expression Expression deriving (Show, Eq)

freeVariables :: Expression -> [Char]
freeVariables (Variable name)      = [name]
freeVariables (Abstraction name t) = freeVariables t \\ [name]
freeVariables (Application s t)    = freeVariables s ++ freeVariables t

boundVariables :: Expression -> [Char]
boundVariables (Variable _)         = []
boundVariables (Abstraction name t) = name : boundVariables t
boundVariables (Application s t)    = boundVariables s ++ boundVariables t 

substitute :: Char -> Expression -> Expression -> Expression
substitute name to from@(Variable name') = if name == name' then to else from
substitute name to from@(Abstraction name' t)
    | name == name'        = from
    | name' `notElem` fvTo = Abstraction name' $ substitute name to t
    | otherwise            = substitute name to from'
    where
        fvTo        = freeVariables to
        conflicting = [x | x <- ['a'..], x `notElem` fvTo ++ boundVariables from] 
        from'       = alphaConversion from conflicting
substitute name to from@(Application s t) =
    Application s' t'
    where 
        s' = substitute name to s
        t' = substitute name to t

alphaConversion :: Expression -> [Char] -> Expression
alphaConversion t@(Abstraction name _) available =
    alphaConversion' t name to
    where to = head $ available \\ boundVariables t

alphaConversion' :: Expression -> Char -> Char -> Expression
alphaConversion' (Variable name) from to = 
    if name == from then Variable to else Variable name
alphaConversion' (Abstraction name t) from to =
    if name == from then Abstraction to t' else Abstraction name t'
    where t' = alphaConversion' t from to
alphaConversion' (Application s t) from to =
    Application s' t'
    where
        s' = alphaConversion' s from to
        t' = alphaConversion' t from to

-- Testes
q :: Expression
q = Abstraction 'y' (Application (Variable 'x') (Variable 'y'))

r :: Expression
r = Application (Variable 'y') (Variable 'z')

s :: Expression
s = substitute 'x' r q