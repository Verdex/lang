
module Main where

import Parse
import ParseLang
import ParseAst
import LuaCompile

import System.Environment

compile i = let (res, _) = parse getAssignments $ makeParseString i in
    case res of
        Just r -> compileToLua r
        Nothing -> "Failure"

main = do
    inputFileName <- fmap (head) getArgs 
    input <- readFile inputFileName
    output <- writeFile "output.lua" $ compile input
    return ()
