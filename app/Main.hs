{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Attoparsec.Text (parseOnly)
import ExprParser (exprParser)
import Evaluate (evalWithErrorThrowing)
import Mylib
import Lib2
import Lib4

main :: IO ()
main = do
    defMain2

