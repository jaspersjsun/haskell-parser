{-# LANGUAGE OverloadedStrings #-}

module Main (main) where


import Control.Applicative ((<|>), empty)
import Control.Monad.State (StateT, get, put, runStateT)
import System.Environment (getArgs)
import Executor

main :: IO ()
main = do
    args <- getArgs
    parseArgs args
    print $ runStateT parseOption args


data Option = Option {
    inPath :: String,
    outPath :: String
}
    deriving Show

type Parser a = StateT [String] Maybe a

parseFlag :: String -> Parser String
parseFlag f = do
    args <- get
    case args of
        [] -> empty
        (arg : args')
            | arg == "--" ++ f -> do
                put args'
                return f
            | otherwise -> empty

parseField :: String -> Parser String
parseField f = do
    parseFlag f
    args <- get
    case args of
        [] -> empty
        (arg : args') -> do
            put args'
            return arg

parseInPath :: Parser String
parseInPath = parseField "in"

parseOutPath :: Parser String
parseOutPath = parseField "out"

parseOption :: Parser Option
parseOption = p0 <|> p1 where
    p0 = do
        i <- parseInPath
        o <- parseOutPath
        return (Option i o)
    p1 = do
        o <- parseOutPath
        i <- parseInPath
        return (Option i o)


parseArgs :: [String] -> IO()
parseArgs xs 
    |length xs == 0 = do
        putStrLn $ "arg is wrong!"
    |xs!!0 == "-repl" = defMain
    |length xs == 1 || length xs == 3 = do
        putStrLn $ "arg is wrong!"
    |length xs == 2 && xs!!0 == "-i" = defMain_file2 (xs!!1)
    |length xs == 2 && xs!!0 == "-t" = defMain_file4 (xs!!1)
    |length xs > 2 && xs!!0 == "-i" && xs!!2 == "-o" = defMain_file1 (xs!!1) (xs!!3)
    |length xs > 2 && xs!!0 == "-t" && xs!!2 == "-o" = defMain_file5 (xs!!1) (xs!!3)
    |otherwise = do
        putStrLn $ "arg is wrong!"
