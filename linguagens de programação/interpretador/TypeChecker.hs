module TypeChecker where

import Lexer

typeof :: Expr -> Maybe Ty
typeof (Num _) = Just TNum
typeof BFalse = Just TBool
typeof BTrue = Just TBool
typeof (Add e1 e2) = case (typeof e1, typeof e2) of
                        (Just TNum, Just TNum) -> Just TNum
                        _                      -> Nothing
typeof (If e1 e2 e3) = 
    case (typeof e1) of
        Just TBool -> case (typeof e2, typeof e3) of
                        (Just t1, Just t2) | t1 == t2 -> Just t1
                                           | otherwise -> Nothing
                        _ -> Nothing
        _ -> Nothing

typecheck :: Expr -> Bool
typecheck = undefined
