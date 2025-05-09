module Lexer where

data Expr = BTrue
        | Bfalse
        | Num Int
        | Add Expr Expr
        | And Expr Expr
        | If Expr Expr Expr
        deriving Show
