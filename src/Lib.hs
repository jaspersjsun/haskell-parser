{-# LANGUAGE OverloadedStrings #-}

module Lib where

import qualified Data.Map as Map

type Var = String

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

    -- Reference the value of a variable. If the variable doesn't exist, it's an error
    | VarRef Var
    deriving Show

-- data type for evaluation result

data Val
    = BoolVal Bool
    | NumVal Double
    | CharVal Char
    | NilVal
    | ConsVal Val Val
    deriving Show

-- data type for statement

data Stmt

    -- Execute a list of statements from left to right
    = StmtList [Stmt]

    -- Evaluate an expression, assign the result value to a variable; create the variable if it doesn't exist
    | VarSet Var Expr

    -- Evaluate the expression, if result is true, execute the left statement, otherwise if it's false, execute the right statement. If the expression doesn't return a boolean, it's an error
    | If Expr Stmt Stmt

    -- Repeatedly evaluate the expression, if result is true then execute the statement and repeat. The expression must return a boolean
    | While Expr Stmt

    -- Skip out one level of "while" loop. It's an error if currently we are not in a loop
    | Skip



-- A program is a single statement
type Prog = Stmt

-- A memory is a mapping from variable names to values
type Mem = Map.Map Var Val

-- -- Evaluation function
-- -- Given an initial memory, execute program and return the memory afterwards
-- eval :: Prog -> Mem -> Mem
-- eval = undefined
