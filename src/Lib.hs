{-# LANGUAGE OverloadedStrings #-}

module Lib where

-- data type for expression

data Expr

    -- Constructors for "Boolean expression"

    = FalseLit
    | TrueLit
    | Not Expr
    | And Expr Expr
    | Or Expr Expr
    
    -- Constructors for "Floating-point expression"
    
    | NumLit Double
    | Add Expr Expr
    | Minus Expr Expr
    | Mult Expr Expr
    | Div Expr Expr
    
    -- Constructors for "Floating-point comparison"
    
    | Eq Expr Expr
    | Less Expr Expr
    | LessEq Expr Expr
    | Greater Expr Expr
    | GreaterEq Expr Expr
    
    -- Constructors for "Lists & Strings"
    -- We don't have "StringLit" here, because string literals are translated to lists of char literals
    
    | CharLit Char
    | Nil
    | Cons Expr Expr
    | Car Expr
    | Cdr Expr

    deriving Show

-- data type for evaluation result

data Result

    = BoolResult Bool
    | NumResult Double
    | NilResult
    | ConsResult Result Result
    
    deriving Show
