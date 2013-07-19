module Main (main) where

import System.Environment
import System.IO
import Lexer
import Parser
import Configuration
import Statement
import Ast

main = do
    params <- getArgs
    let filename = head params
        input = map read $ tail params :: [Int]
    withFile filename ReadMode $ run input

run input handle = do
    s <- hGetContents handle
    let ast0 = parse $ scan s
        ast = normalize ast0
    putStrLn $ show $ output $ evaluate ast $ new input
    putStr $ show ast
