{-# LANGUAGE OverloadedStrings #-}

module Lib4 where
import qualified Data.Map as M
import ExprParser
import Evaluate
import Data.Attoparsec.Text
import System.IO
import Data.Text
import Lib (Expr(..), Val(..))

type Env = M.Map String String

fromRight::Either a b -> b
fromRight (Right x) = x
-- mymainkoop ::Handle -> String -> Env -> IO ()
-- mymainkoop ouh ss env = do
--     case Prelude.words ls of
--         [":q"] -> do
--             putStrLn "Bye Bye~"
--         _ -> do
--             hPutStrLn ouh  (show $ evalWithErrorThrowing $ parseOnly exprParser (pack ls))

mainLoop :: Either String Expr -> Env -> IO ()
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
            if (exps /= (Left ""))
            then do
                putStrLn $ show (fromRight exps)
                mainLoop exps env
            else
                mainLoop exps env
        _ -> do
            let exps = parseOnly exprParser (pack ls)
            putStrLn $ show $ evalWithErrorThrowing $ parseOnly exprParser (pack ls)
            mainLoop exps env
    --interact test
    
    -- interact (evalWithErrorThrowing $ (parseOnly exprParser))
    -- interact test
        -- ["set",var,val] -> do
        --     putStrLn (var ++ " is set to " ++ val)
        --     mainLoop (M.insert var val env)
        -- ["view",var] -> case M.lookup var env of
        --     Just val -> do
        --         putStrLn (var ++ " = " ++ val)
        --         mainLoop env
        --     Nothing -> do
        --         putStrLn "variable not found!"
        --         mainLoop env
        -- ["exit"] -> putStrLn "Bye~"
    -- do
    --     putStrLn "unrecognized command!"
    
-- test ::Text
-- test =   "(not True)"
defMain_file1 :: String -> String -> IO()  --（for -i -o）
defMain_file1 ins outs = do
    inh <- openFile ins ReadMode
    ouh <- openFile outs WriteMode
    processLine1 inh ouh (M.empty)
    hClose inh
    hClose ouh

defMain_file5 :: String -> String -> IO()  --（for -t -o）
defMain_file5 ins outs = do
    inh <- openFile ins ReadMode
    ouh <- openFile outs WriteMode
    processLine4 inh ouh (M.empty)
    hClose inh
    hClose ouh

defMain_file2 :: String -> IO()--(for -i )
defMain_file2 ins = do
    inh <- openFile ins ReadMode
    processLine2 inh (M.empty)
    hClose inh

defMain_file4 :: String -> IO()--(for -t)
defMain_file4 ins = do
    inh <- openFile ins ReadMode
    processLine3 inh (M.empty)
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
                           hPutStrLn ouh  (show $ evalWithErrorThrowing $ parseOnly exprParser (pack lineStr))
                           processLine1 inh ouh env

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
                           putStrLn  (show $ evalWithErrorThrowing $ parseOnly exprParser (pack lineStr))
                           processLine2 inh env

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
                           if (parseOnly exprParser (pack lineStr) /= Left "")
                           then do
                               putStrLn  (show $  fromRight (parseOnly exprParser (pack lineStr)))
                               processLine3 inh env
                           else
                               processLine3 inh env

processLine4 :: Handle -> Handle -> Env -> IO ()
processLine4 inh ouh env =
    do isEof <- hIsEOF inh
       if isEof
           then return ()
           else do lineStr <- hGetLine inh
                   case Prelude.words lineStr of
                       _ -> do
                           if (parseOnly exprParser (pack lineStr) /= (Left ""))
                           then do
                               hPutStrLn ouh  (show $ fromRight (parseOnly exprParser (pack lineStr)))
                               processLine4 inh ouh env
                           else
                               processLine4 inh ouh env

defMain :: IO ()
defMain = do
    -- putStrLn $ show $ evalWithErrorThrowing $ (parseOnly exprParser test)
    Prelude.putStrLn "This is a simple REPL. Be my guest!"
    mainLoop (Left "") (M.empty)
