module Bruijn where

import           Data.List  (elemIndex, (\\))
import           Expression (Expression (evalStep))
import qualified Lambda     as Named (Exp (Abs, App, Var), freeVariables)

type Gamma = [Char]

data Exp
  = Var Int
  | Abs Exp
  | App Exp Exp
  deriving (Eq)

instance Show Exp where
  show :: Exp -> String
  show (Var index) = show index
  show (Abs t)     = "(λ . " ++ show t ++ ")"
  show (App s t)   = "(" ++ show s ++ " " ++ show t ++ ")"

instance Expression Exp where
  evalStep :: Exp -> Exp
  evalStep (App (Abs body) arg)
    | isValue arg = shift (subs body 0 (shift arg 1 0)) (-1) 0
    | otherwise   = App (Abs body) arg'
    where arg' = evalStep arg
  evalStep (App s t) = let s' = evalStep s in App s' t
  evalStep t = t

isValue:: Exp -> Bool
isValue (Var _) = True
isValue (Abs _) = True
isValue _       = False

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
shift (Var num) d cutoff = Var $ if num >= cutoff then num + d else num
shift (Abs t)   d cutoff = Abs $ shift t d (cutoff + 1)
shift (App s t) d cutoff = App (shift s d cutoff) (shift t d cutoff)

subs :: Exp -> Int -> Exp -> Exp
subs (Var k)   j by = if k == j then by else Var k
subs (Abs t)   j by = Abs $ subs t (j + 1) (shift by 1 0)
subs (App s t) j by = App (subs s j by) (subs t j by)
