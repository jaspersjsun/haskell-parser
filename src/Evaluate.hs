{-# LANGUAGE OverloadedStrings #-}

-- combine the 'eval' functions into one module

module Evaluate where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Functor
import Lib

eval :: Expr -> Lib.Result
eval FalseLit = BoolResult False
eval TrueLit = BoolResult True
eval (Not p) = BoolResult $ not $ evalBool p
eval (And p q) = BoolResult $ evalBool p && evalBool q
eval (Or p q) = BoolResult $ evalBool p || evalBool q

fromBoolResult :: Lib.Result -> Bool
fromBoolResult (BoolResult b) = b
fromBoolResult _ = errorWithoutStackTrace "Evaluate.fromBoolResult: Not BoolResult"

evalBool :: Expr -> Bool
evalBool = fromBoolResult . eval

evalWithErrorThrowing :: Either String Expr -> String
evalWithErrorThrowing (Left errStr) = "not a valid bool expr: " ++ errStr
evalWithErrorThrowing (Right expr) = show $ eval expr
