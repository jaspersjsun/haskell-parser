{-# LANGUAGE OverloadedStrings #-}

-- combine the 'exprParse' functions into one module

module Parse where

import Control.Applicative
import Data.Attoparsec.Text
import Lib
import Boolean ( boolParser )

exprParser :: Parser Expr
exprParser = boolParser
