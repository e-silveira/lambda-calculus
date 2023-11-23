module Bruijn where

import qualified Lambda as Named (Exp(Var, Abs, App), freeVariables)
import Data.List (elemIndex, (\\))

type Gamma = [Char]

data Exp
  = Var Int
  | Abs Exp
  | App Exp Exp
  deriving (Eq)

instance Show Exp where
    show :: Exp -> String
    show (Var index) = show index
    show (Abs t) = "(λ . " ++ show t ++ ")"
    show (App s t) = "(" ++ show s ++ " " ++ show t ++ ")"

removeNames :: Named.Exp -> Gamma -> Exp
removeNames (Named.Var name) gamma = Var index where (Just index) = elemIndex name gamma
removeNames (Named.Abs name t) gamma = Abs $ removeNames t (name:gamma)
removeNames (Named.App s t) gamma = App (removeNames s gamma) (removeNames t gamma)

restoreNames :: Exp -> Gamma -> Named.Exp
restoreNames (Var index) gamma = Named.Var $ gamma !! index
restoreNames (Abs t) gamma = 
  Named.Abs name (restoreNames t $ name:gamma)
  where name = head $ ['a'..] \\ gamma 
restoreNames (App s t) gamma = Named.App (restoreNames s gamma) (restoreNames t gamma)

shift :: Exp -> Int -> Int -> Exp
shift t@(Var num) d cutoff = Var $ if num >= cutoff then num + d else num
shift (Abs t) d cutoff = Abs $ shift t d (cutoff + 1)
shift (App s t) d cutoff = App (shift s d cutoff) (shift t d cutoff)

-- Usar Var ou Int?
subs :: Exp -> Exp -> Exp -> Exp
subs orig@(Var k) (Var j) by = if k == j then by else orig
subs (Abs t) (Var j) by =
  Abs $ subs t (Var $ j + 1) (shift by 1 0)
subs (App s t) j by =
  App (subs s j by) (subs t j by)

isValue:: Exp -> Bool
isValue(Var _) = True
isValue(Abs _) = True
isValue(App _ _) = False

eval' :: Exp -> Exp
eval' (App (Abs t12) t2) = 
  if isValue t2
    then shift (subs t12 (Var 0) (shift t2 1 0)) (-1) 0
    else let t2' = eval' t2 in App (Abs t12) t2'
eval' (App t1 t2) = let t1' = eval' t1 in App t1' t2
eval' t = t

eval :: Exp -> Exp
eval t = if t == eval' t then t else eval (eval' t)

evalWithBruijn :: Named.Exp -> Named.Exp
evalWithBruijn t = 
  restoreNames (eval $ removeNames t gamma) gamma
  where gamma = Named.freeVariables t

