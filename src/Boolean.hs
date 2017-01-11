{-# LANGUAGE OverloadedStrings #-}

-- store constructors and converters for boolean data

module Boolean ( boolParser, fromBoolResult ) where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Functor
import Lib

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
    expr <- boolParser
    lexeme $ char ')'
    return (Not expr)

andParser :: Parser Expr
andParser = do
    lexeme $ char '('
    lexeme $ string "and"
    expr1 <- boolParser
    expr2 <- boolParser
    lexeme $ char ')'
    return (And expr1 expr2)

orParser :: Parser Expr
orParser = do
    lexeme $ char '('
    lexeme $ string "or"
    expr1 <- boolParser
    expr2 <- boolParser
    lexeme $ char ')'
    return (Or expr1 expr2)

lexeme :: Parser a -> Parser a
lexeme p = do
    skipSpace
    p

fromBoolResult :: Lib.Result -> Bool
fromBoolResult (BoolResult b) = b
fromBoolResult _ = errorWithoutStackTrace "Evaluate.fromBoolResult: Not BoolResult"
