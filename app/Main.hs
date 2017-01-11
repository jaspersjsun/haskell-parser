{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Attoparsec.Text (parseOnly)
import Parse (exprParser)
import Evaluate (evalWithErrorThrowing)

main :: IO ()
main = do
    putStrLn $ show $ evalWithErrorThrowing $ parseOnly exprParser "(not True)"
    putStrLn $ show $ evalWithErrorThrowing $ parseOnly exprParser "\"hello\""
    putStrLn $ show $ evalWithErrorThrowing $ parseOnly exprParser "(nXXX True)"
    putStrLn $ show $ evalWithErrorThrowing $ parseOnly exprParser "(not True)   MORE"
    putStrLn $ show $ evalWithErrorThrowing $ parseOnly exprParser "(not (and True False))   MORE"
