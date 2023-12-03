module Expression where

import Control.Monad.Writer (Writer, runWriter, tell)

class
  (Show a, Eq a) =>
  Expression a
  where
  eval :: a -> a
  eval t =
    if t == t'
      then t
      else eval t'
    where
      t' = evalStep t
  evalStep :: a -> a
  evalAccum :: a -> Writer [a] a
  evalAccum e = do
    let e' = evalStep e
    tell [e]
    if e == e'
      then return e
      else evalAccum e'
