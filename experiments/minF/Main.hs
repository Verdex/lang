
module Main where

import System.Environment
import System.IO

import LangAst
import Tokenizer
import LangParser

main = 
    do
        arg : _ <- getArgs
        contents <- readFile arg
        putStrLn contents 
