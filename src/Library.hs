module Library where

import Lambda

true :: LamExp
true = LamAbs 'x' (LamAbs 'y' (LamVar 'x'))

false :: LamExp
false = LamAbs 'x' (LamAbs 'y' (LamVar 'y'))

and :: LamExp
and = LamAbs 'b' (LamAbs 'c' (LamApp (LamApp (LamVar 'b') (LamVar 'c')) false))

or :: LamExp
or = LamAbs 'b' (LamAbs 'c' (LamApp (LamApp (LamVar 'b') true) (LamVar 'c')))