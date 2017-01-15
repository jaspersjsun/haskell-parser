{-# LANGUAGE OverloadedStrings #-}

module Executor where

import qualified Data.Map as Map
import ExprParser
import StmtParser
import StmtEvaluator
import ExprEvaluator
import Data.Attoparsec.Text
import System.IO
import Data.Either
import Data.Text
import Lib

type Env = Mem

fromRight :: Either a b -> b
fromRight (Right x) = x

fromLeft :: Either a b -> a
fromLeft (Left x) = x

mainLoop :: Either String (Either Stmt Expr) -> Env -> IO ()
mainLoop exps env = do
    putStr "> "
    hFlush stdout
    ls <- getLine
    case Prelude.words ls of
        [":q"] -> do
            putStrLn "Bye Bye~"
        [":i",file] -> do
            defMain_file3 env file
            mainLoop exps env
        [":t"] -> do
            putStrLn (if isLeft exps then fromLeft exps else (if isLeft $ fromRight exps then show $ fromLeft $ fromRight exps else show $ fromRight $ fromRight exps))
            mainLoop exps env
        _ -> do
            let stmtParseResult = parseOnly stmtParser (pack ls)
            if isLeft stmtParseResult
                then do
                    let exprParseResult = parseOnly exprParser (pack ls)
                    if isLeft exprParseResult
                        then do
                            putStrLn $ "not a valid experssion or statemet"
                            mainLoop exps env
                        else do
                            putStrLn $ show $ eval env (fromRight exprParseResult)
                            let exps' = (Right $ Right $ fromRight exprParseResult)
                            mainLoop exps env
                else do
                    let exps' = (Right $ Left $ fromRight stmtParseResult)
                    let env' = evalProg (fromRight stmtParseResult) env
                    mainLoop exps' env'

defMain_file1 :: String -> String -> IO()  --（for -i -o）
defMain_file1 ins outs = do
    inh <- openFile ins ReadMode
    ouh <- openFile outs WriteMode
    processLine1 inh ouh (Map.empty)
    hClose inh
    hClose ouh

defMain_file5 :: String -> String -> IO()  --（for -t -o）
defMain_file5 ins outs = do
    inh <- openFile ins ReadMode
    ouh <- openFile outs WriteMode
    processLine4 inh ouh (Map.empty)
    hClose inh
    hClose ouh

defMain_file2 :: String -> IO()--(for -i )
defMain_file2 ins = do
    inh <- openFile ins ReadMode
    processLine2 inh (Map.empty)
    hClose inh

defMain_file4 :: String -> IO()--(for -t)
defMain_file4 ins = do
    inh <- openFile ins ReadMode
    processLine3 inh (Map.empty)
    hClose inh

defMain_file3 :: Env -> String -> IO()--(for :i)
defMain_file3 env ins = do
    inh <- openFile ins ReadMode
    processLine2 inh (env)
    hClose inh

processLine1 :: Handle -> Handle -> Env -> IO ()
processLine1 inh ouh env =
    do isEof <- hIsEOF inh
       if isEof
           then return ()
           else do lineStr <- hGetLine inh
                   case Prelude.words lineStr of
                       [":q"] -> do
                           hPutStrLn ouh "Bye Bye~"
                       _ -> do
                           let stmtParseResult = parseOnly stmtParser (pack lineStr)
                           if isLeft stmtParseResult
                               then do
                                   let exprParseResult = parseOnly exprParser (pack lineStr)
                                   if isLeft exprParseResult
                                       then do
                                           hPutStrLn ouh "not a valid experssion or statemet"
                                           processLine1 inh ouh env
                                       else do
                                           hPutStrLn ouh $ show $ eval env (fromRight exprParseResult)
                                           processLine1 inh ouh env
                               else do
                                   let env' = evalProg (fromRight stmtParseResult) env
                                   processLine1 inh ouh env'

processLine2 :: Handle ->  Env -> IO ()
processLine2 inh env =
    do isEof <- hIsEOF inh
       if isEof
           then return ()
           else do lineStr <- hGetLine inh
                   case Prelude.words lineStr of
                       [":q"] -> do
                           putStrLn "Bye Bye~"
                       _ -> do
                           let stmtParseResult = parseOnly stmtParser (pack lineStr)
                           if isLeft stmtParseResult
                               then do
                                   let exprParseResult = parseOnly exprParser (pack lineStr)
                                   if isLeft exprParseResult
                                       then do
                                           putStrLn "not a valid experssion or statemet"
                                           processLine2 inh env
                                       else do
                                           putStrLn $ show $ eval env (fromRight exprParseResult)
                                           processLine2 inh env
                               else do
                                   let env' = evalProg (fromRight stmtParseResult) env
                                   processLine2 inh env'

processLine3 :: Handle ->  Env -> IO ()
processLine3 inh env =
    do isEof <- hIsEOF inh
       if isEof
           then return ()
           else do lineStr <- hGetLine inh
                   case Prelude.words lineStr of
                       [":q"] -> do
                           putStrLn "Bye Bye~"
                       _ -> do
                           let stmtParseResult = parseOnly stmtParser (pack lineStr)
                           if isLeft stmtParseResult
                               then do
                                   let exprParseResult = parseOnly exprParser (pack lineStr)
                                   if isLeft exprParseResult
                                       then do
                                           putStrLn "not a valid experssion or statemet"
                                           processLine3 inh env
                                       else do
                                           putStrLn $ show (fromRight exprParseResult)
                                           processLine3 inh env
                               else do
                                   putStrLn $ show (fromRight stmtParseResult)
                                   processLine3 inh env

processLine4 :: Handle -> Handle -> Env -> IO ()
processLine4 inh ouh env =
    do isEof <- hIsEOF inh
       if isEof
           then return ()
           else do lineStr <- hGetLine inh
                   case Prelude.words lineStr of
                       [":q"] -> do
                           hPutStrLn ouh "Bye Bye~"
                       _ -> do
                           let stmtParseResult = parseOnly stmtParser (pack lineStr)
                           if isLeft stmtParseResult
                               then do
                                   let exprParseResult = parseOnly exprParser (pack lineStr)
                                   if isLeft exprParseResult
                                       then do
                                           hPutStrLn ouh "not a valid experssion or statemet"
                                           processLine4 inh ouh env
                                       else do
                                           hPutStrLn ouh $ show $ fromRight exprParseResult
                                           processLine4 inh ouh env
                               else do
                                   hPutStrLn ouh $ show $ fromRight stmtParseResult
                                   processLine4 inh ouh env

defMain :: IO ()
defMain = do
    Prelude.putStrLn "This is a simple REPL. Be my guest!"
    mainLoop (Left "") (Map.empty)
