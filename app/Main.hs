module Main (main) where

import Lib (compile)

import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> error "No source file provided!\nTo compile a program, make sure to provide a path as an argument. E.g.:\nstack run -- my_program.mhl"
        [path] -> compile path
        _ -> error "Only one argument should be provided. E.g.:\nstack run -- my_program.mhl"
