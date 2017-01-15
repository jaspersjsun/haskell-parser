{-# LANGUAGE OverloadedStrings #-}

-- combine the 'eval' functions into one module

module Evaluate where

import Lib (Expr(..), Val(..), Mem)
import qualified Data.Map as Map
import Data.Maybe (isNothing, fromJust)
eval :: Mem -> Expr -> Val

-- | eval bool expression
eval _ FalseLit  = BoolVal False
eval _ TrueLit   = BoolVal True
eval m (Not p)   = BoolVal $ not $ evalBool m p
eval m (And p q) = BoolVal $ evalBool m p && evalBool m q
eval m (Or p q)  = BoolVal $ evalBool m p || evalBool m q

-- | eval floating-point expression
eval _ (NumLit x)  = NumVal x
eval m (Add p q)   = NumVal $ evalNum m p + evalNum m q
eval m (Minus p q) = NumVal $ evalNum m p - evalNum m q
eval m (Mult p q)  = NumVal $ evalNum m p * evalNum m q
eval m (Div p q)   = NumVal $ evalNum m p / evalNum m q

-- | eval floating-point comparison
eval m (Eq p q)        = BoolVal $ evalNum m p == evalNum m q
eval m (Less p q)      = BoolVal $ evalNum m p < evalNum m q
eval m (LessEq p q)    = BoolVal $ evalNum m p <= evalNum m q
eval m (Greater p q)   = BoolVal $ evalNum m p > evalNum m q
eval m (GreaterEq p q) = BoolVal $ evalNum m p >= evalNum m q

-- | eval list and char
eval _ Nil              = NilVal
eval _ (CharLit c)      = CharVal c
eval m (Cons p q)       = ConsVal (eval m p) (eval m q)
eval m (Car (Cons p q)) = eval m p
eval m (Cdr (Cons p q)) = eval m q

-- | eval variable
eval m (VarRef v) = 
    if isNothing var
        then error $ "lookup failed! no variable named " ++ v
        else fromJust var
    where var = Map.lookup v m

fromBoolVal :: Val -> Bool
fromBoolVal (BoolVal b) = b
fromBoolVal _ = errorWithoutStackTrace "Evaluate.fromBoolVal: Not BoolVal"

evalBool :: Mem -> Expr -> Bool
evalBool m e = fromBoolVal $ eval m e

fromNumVal :: Val -> Double
fromNumVal (NumVal b) = b
fromNumVal _ = errorWithoutStackTrace "Evaluate.fromNumVal: Not NumVal"

evalNum :: Mem -> Expr -> Double
evalNum m e = fromNumVal $ eval m e

evalWithErrorThrowing :: Mem -> Either String Expr -> String
evalWithErrorThrowing _ (Left errStr) = "not a valid bool expr: " ++ errStr
evalWithErrorThrowing m (Right expr) = show $ eval m expr
