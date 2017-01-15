{-# LANGUAGE OverloadedStrings #-}

-- combine the 'exprParse' functions into one module

module StmtParser where

import Control.Applicative ((<|>))
import Data.Attoparsec.Text (Parser, skipSpace, char, double, string, anyChar, takeWhile1, peekChar)
import Data.Functor (($>))
import Data.Text (unpack)
import Lib
import ExprParser (exprParser, lexeme, varParser)
import Data.Maybe (isJust, fromJust)

stmtParser :: Parser Stmt
stmtParser = setParser <|> ifParser <|> whileParser <|> skipParser <|> stmtListParser

setParser :: Parser Stmt
setParser = do
    lexeme $ char '('
    lexeme $ string "set!"
    varVal <- varParser
    let var = fromVarVal varVal
    expr <- exprParser
    lexeme $ char ')'
    return (VarSet var expr)

ifParser :: Parser Stmt
ifParser = do
    lexeme $ char '('
    lexeme $ string "if"
    expr <- exprParser
    stmt1 <- stmtParser
    stmt2 <- stmtParser
    lexeme $ char ')'
    return (If expr stmt1 stmt2)

whileParser :: Parser Stmt
whileParser = do
    lexeme $ char '('
    lexeme $ string "while"
    expr <- exprParser
    stmt <- stmtParser
    lexeme $ char ')'
    return (While expr stmt)

skipParser :: Parser Stmt
skipParser = lexeme $ string "skip" $> Skip

stmtListParser :: Parser Stmt
stmtListParser = do
    lexeme $ char '('
    lexeme $ string "begin"
    stmts <- multiStmtParser
    lexeme $ char ')'
    return (StmtList stmts)

multiStmtParser :: Parser [Stmt]
multiStmtParser = do
    skipSpace
    peekchr <- peekChar
    if (isJust peekchr && (fromJust peekchr) /= ')')
        then do
            stmt <- stmtParser
            stmts <- multiStmtParser
            return (stmt:stmts)
        else return []


fromVarVal :: Expr -> Var
fromVarVal (VarRef var) = var
