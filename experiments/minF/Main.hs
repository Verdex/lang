
module Main where

import System.Environment
import System.IO

import Control.Applicative

import LangAst
import Tokenizer
import LangParser

main = 
    do
        arg : _ <- getArgs
        contents <- readFile arg
        toks <- pure $ tokenize contents
        putStrLn $ show toks
        prog <- pure $ parse toks
        putStrLn $ show prog 
