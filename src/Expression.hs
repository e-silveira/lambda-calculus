module Expression where

class Eq a => Expression a where
    eval :: a -> a
    eval t = if t == t' then t else eval t' where t' = evalStep t
    
    evalStep :: a -> a


