{-# LANGUAGE OverloadedStrings #-}

-- combine the 'exprParse' functions into one module

module Parse where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Functor
import Lib

exprParser :: Parser Expr
exprParser = boolParser

boolParser :: Parser Expr
boolParser = falseParser <|> trueParser <|> notParser <|> andParser <|> orParser

falseParser :: Parser Expr
falseParser = lexeme $ string "False" $> FalseLit

trueParser :: Parser Expr
trueParser = lexeme $ string "True" $> TrueLit

notParser :: Parser Expr
notParser = do
    lexeme $ char '('
    lexeme $ string "not"
    expr <- exprParser
    lexeme $ char ')'
    return (Not expr)

andParser :: Parser Expr
andParser = do
    lexeme $ char '('
    lexeme $ string "and"
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ char ')'
    return (And expr1 expr2)

orParser :: Parser Expr
orParser = do
    lexeme $ char '('
    lexeme $ string "or"
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ char ')'
    return (Or expr1 expr2)

lexeme :: Parser a -> Parser a
lexeme p = do
    skipSpace
    p
