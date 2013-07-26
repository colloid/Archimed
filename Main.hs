module Main (main) where

import System.Environment
import System.IO
import Lexer
import Parser
import Configuration
import Statement
import Ast

usage = concat
    [ "USAGE:\n"
    , "    archimed run PROGRAM [ INPUT... ]\n"
    , "    archimed mix PROGRAM [ INPUT... ]\n"
    , "    archimed opt PROGRAM"
    ]    

data Param = Run String [Int]
           | Mix String [Input]
           | Opt String

getPName (Run n _) = n
getPName (Mix n _) = n
getPName (Opt n  ) = n
    
parseParams ("run":progname:input) = Run progname $ map read input
parseParams ("mix":progname:input) = Mix progname $ map read input
parseParams ("opt":progname:[])    = Opt progname
parseParams _ = error usage

main = do
    params <- fmap parseParams getArgs
    withFile (getPName params) ReadMode $ execute params

execute params handle = do
    file <- hGetContents handle
    let ast = parse $ scan file
    execute' params ast

execute' (Run progname input) ast = do
    let conf = evaluate ast $ new $ map Def input
        res  = case Configuration.input conf of
            [] -> show $ output conf
            _  -> error "too many input data"
    putStrLn res

execute' (Mix progname input) ast = do
    let res = show $ normalize $ fst $ mix ast $ new input
    putStrLn res

execute' (Opt progname) ast = do
    putStr $ show $ normalize ast
