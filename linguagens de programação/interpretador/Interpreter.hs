module Interpreter where

import Lexer

isValue :: Expr -> Bool
isValue = undefined

step :: Expr -> Expr
step (Add (Num n1) (Num n2)) = Num (n1+n2)
step (Add (Num n1) e2) = let e2' = step e2
                        in Add (Num n1) e2'
step (Add e1 e2) = Add (step e1) e2
step (And BTrue e2) = e2
step (And BFalse e2) = BFalse
step (And e1 e2) = And (step e1) e2

step (If BTrue e1 e2) = e1
step (If BFalse e1 e2) = e2
step (If e e1 e2) = If (step e) e1 e2

eval :: Expr -> Bool
eval = undefined
