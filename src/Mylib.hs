{-# LANGUAGE OverloadedStrings #-}

module Mylib where
import System.IO
--import Data.Text
import Lib4



mygetArgs::[String] -> IO()
mygetArgs xs 
    |length xs == 0 = do
    	putStrLn $ "arg is wrong!"
    |xs!!0 == "-repl" = defMain
    |length xs == 1 || length xs == 3 = do
    	putStrLn $ "arg is wrong!"
    |length xs == 2 && xs!!0 == "-i" = defMain_file2 (xs!!1)
    |length xs == 2 && xs!!0 == "-t" = defMain_file4 (xs!!1)
    |length xs > 2 && xs!!0 == "-i" && xs!!2 == "-o" = defMain_file1 (xs!!1) (xs!!3)
    |length xs > 2 && xs!!0 == "-t" && xs!!2 == "-o" = defMain_file1 (xs!!1) (xs!!3)
    |otherwise = do
    	putStrLn $ "arg is wrong!"