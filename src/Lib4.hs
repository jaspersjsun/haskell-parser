{-# LANGUAGE OverloadedStrings #-}

module Lib4 where
import qualified Data.Map as M
import Parse
import Evaluate
import Data.Attoparsec.Text
import System.IO
import Data.Text

type Env = M.Map String String

-- test :: Text -> Text
-- test xs = evalWithErrorThrowing $ (parseOnly exprParser xs)

mytest :: Text -> String
mytest xs = unpack xs

mytest2 :: String -> Text
mytest2 xs = pack xs

-- test :: String -> String
-- test xs = evalWithErrorThrowing $ parseOnly exprParser (pack xs)
test :: String -> String
test xs = unpack(pack xs)

test2 :: String
test2 = "(not True)"

-- mymainkoop ::Handle -> String -> Env -> IO ()
-- mymainkoop ouh ss env = do
--     case Prelude.words ls of
--         [":q"] -> do
--             putStrLn "Bye Bye~"
--         _ -> do
--             hPutStrLn ouh  (show $ evalWithErrorThrowing $ parseOnly exprParser (pack ls))

mainLoop :: Env -> IO ()
mainLoop env = do
    putStr "> "
    hFlush stdout
    ls <- getLine
    case Prelude.words ls of
        [":q"] -> do
            putStrLn "Bye Bye~"
        _ -> do
            putStrLn $ show $ evalWithErrorThrowing $ parseOnly exprParser (pack ls)
            mainLoop env
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
defMain_file1 :: String -> String -> IO()
defMain_file1 ins outs = do
    inh <- openFile ins ReadMode
    ouh <- openFile outs WriteMode
    processLine1 inh ouh (M.empty)
    hClose inh
    hClose ouh

defMain_file2 :: String -> IO()
defMain_file2 ins = do
    inh <- openFile ins ReadMode
    processLine2 inh (M.empty)
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

defMain :: IO ()
defMain = do
    -- putStrLn $ show $ evalWithErrorThrowing $ (parseOnly exprParser test)
    Prelude.putStrLn "This is a simple REPL. Be my guest!"
    mainLoop (M.empty)