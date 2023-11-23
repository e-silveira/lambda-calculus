module Lambda where

import Data.List ((\\), elemIndex)

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

toString :: LamExp -> String
toString (LamVar name) = [name]
toString (LamAbs name t) = "(λ " ++ [name] ++ " . " ++ toString t ++ ")"
toString (LamApp s t) = "(" ++ toString s ++ " " ++ toString t ++ ")"

toStringBruijn :: BruijnLamExp -> String
toStringBruijn (BruijnLamVar index) = show index
toStringBruijn (BruijnLamAbs t) = "(λ . " ++ toStringBruijn t ++ ")"
toStringBruijn (BruijnLamApp s t) = "(" ++ toStringBruijn s ++ " " ++ toStringBruijn t ++ ")"

-- Bruijn

-- Para que não seja necessário atualizar os valores quando uma variável entrar em Gamma.
type Gamma = [Char]

{--
findIndex :: Char -> Gamma -> Int
findIndex char gamma = findIndex' char gamma 0

findIndex' :: Char -> Gamma -> Int -> Int
findIndex' char gamma index
  | char == last gamma = index
  | otherwise          = findIndex' char (tail gamma) (succ index)
--}

removeNames :: LamExp -> Gamma -> BruijnLamExp
removeNames (LamVar name) gamma = BruijnLamVar index where (Just index) = elemIndex name gamma
removeNames (LamAbs name t) gamma = BruijnLamAbs $ removeNames t (name:gamma)
removeNames (LamApp s t) gamma = BruijnLamApp (removeNames s gamma) (removeNames t gamma)

restoreNames :: BruijnLamExp -> Gamma -> LamExp
restoreNames (BruijnLamVar index) gamma = LamVar $ gamma !! index
restoreNames (BruijnLamAbs t) gamma = 
  LamAbs name (restoreNames t $ name:gamma)
  where name = head $ ['a'..] \\ gamma 
restoreNames (BruijnLamApp s t) gamma = LamApp (restoreNames s gamma) (restoreNames t gamma)

data BruijnLamExp
  = BruijnLamVar Int
  | BruijnLamAbs BruijnLamExp
  | BruijnLamApp BruijnLamExp BruijnLamExp
  deriving (Show, Eq)

-- parse com nome -> transforma para sem nome -> eval sem nome -> transforma para com nome