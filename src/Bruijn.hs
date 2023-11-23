module Bruijn where

import Lambda (LamExp(LamVar, LamAbs, LamApp), freeVariables)
import Data.List (elemIndex, (\\))

type Gamma = [Char]

data BruijnLamExp
  = BruijnLamVar Int
  | BruijnLamAbs BruijnLamExp
  | BruijnLamApp BruijnLamExp BruijnLamExp
  deriving (Eq)

instance Show BruijnLamExp where
    show :: BruijnLamExp -> String
    show (BruijnLamVar index) = show index
    show (BruijnLamAbs t) = "(λ . " ++ show t ++ ")"
    show (BruijnLamApp s t) = "(" ++ show s ++ " " ++ show t ++ ")"

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

shift :: BruijnLamExp -> Int -> Int -> BruijnLamExp
shift t@(BruijnLamVar num) d cutoff = BruijnLamVar $ if num >= cutoff then num + d else num
shift (BruijnLamAbs t) d cutoff = BruijnLamAbs $ shift t d (cutoff + 1)
shift (BruijnLamApp s t) d cutoff = BruijnLamApp (shift s d cutoff) (shift t d cutoff)

-- Usar BruijnLamVar ou Int?
subs :: BruijnLamExp -> BruijnLamExp -> BruijnLamExp -> BruijnLamExp
subs orig@(BruijnLamVar k) (BruijnLamVar j) by = if k == j then by else orig
subs (BruijnLamAbs t) (BruijnLamVar j) by =
  BruijnLamAbs $ subs t (BruijnLamVar $ j + 1) (shift by 1 0)
subs (BruijnLamApp s t) j by =
  BruijnLamApp (subs s j by) (subs t j by)

isValue:: BruijnLamExp -> Bool
isValue(BruijnLamVar _) = True
isValue(BruijnLamAbs _) = True
isValue(BruijnLamApp _ _) = False

eval' :: BruijnLamExp -> BruijnLamExp
eval' (BruijnLamApp (BruijnLamAbs t12) t2) = 
  if isValue t2
    then shift (subs t12 (BruijnLamVar 0) (shift t2 1 0)) (-1) 0
    else let t2' = eval' t2 in BruijnLamApp (BruijnLamAbs t12) t2'
eval' (BruijnLamApp t1 t2) = let t1' = eval' t1 in BruijnLamApp t1' t2
eval' t = t

eval :: BruijnLamExp -> BruijnLamExp
eval t = if t == eval' t then t else eval (eval' t)

evalWithBruijn :: LamExp -> LamExp
evalWithBruijn t = 
  restoreNames (eval $ removeNames t gamma) gamma
  where gamma = freeVariables t

