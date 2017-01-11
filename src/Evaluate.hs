{-# LANGUAGE OverloadedStrings #-}

-- combine the 'eval' functions into one module

module Evaluate where

import Lib (Expr(..), Val(..))

eval :: Expr -> Val

-- | eval bool expression
eval FalseLit  = BoolVal False
eval TrueLit   = BoolVal True
eval (Not p)   = BoolVal $ not $ evalBool p
eval (And p q) = BoolVal $ evalBool p && evalBool q
eval (Or p q)  = BoolVal $ evalBool p || evalBool q

-- | eval floating-point expression
eval (NumLit x)  = NumVal x
eval (Add p q)   = NumVal $ evalNum p + evalNum q
eval (Minus p q) = NumVal $ evalNum p - evalNum q
eval (Mult p q)  = NumVal $ evalNum p * evalNum q
eval (Div p q)   = NumVal $ evalNum p / evalNum q

-- | eval floating-point comparison
eval (Eq p q)        = BoolVal $ evalNum p == evalNum q
eval (Less p q)      = BoolVal $ evalNum p < evalNum q
eval (LessEq p q)    = BoolVal $ evalNum p <= evalNum q
eval (Greater p q)   = BoolVal $ evalNum p > evalNum q
eval (GreaterEq p q) = BoolVal $ evalNum p >= evalNum q

-- | eval list and char
eval Nil              = NilVal
eval (CharLit c)      = CharVal c
eval (Cons p q)       = ConsVal (eval p) (eval q)
eval (Car (Cons p q)) = eval p
eval (Cdr (Cons p q)) = eval q

fromBoolResult :: Val -> Bool
fromBoolResult (BoolVal b) = b
fromBoolResult _ = errorWithoutStackTrace "Evaluate.fromBoolResult: Not BoolResult"

evalBool :: Expr -> Bool
evalBool = fromBoolResult . eval

fromNumResult :: Val -> Double
fromNumResult (NumVal b) = b
fromNumResult _ = errorWithoutStackTrace "Evaluate.fromNumResult: Not NumResult"

evalNum :: Expr -> Double
evalNum = fromNumResult . eval

evalWithErrorThrowing :: Either String Expr -> String
evalWithErrorThrowing (Left errStr) = "not a valid bool expr: " ++ errStr
evalWithErrorThrowing (Right expr) = show $ eval expr
