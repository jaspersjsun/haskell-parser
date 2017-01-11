{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Attoparsec.Text (parseOnly)
import Parse (exprParser)
import Evaluate (evalWithErrorThrowing)
import Lib4

main :: IO ()
main = defMain
