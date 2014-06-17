
module Main where

import Parse
import ParseLang
import ParseAst
import LuaCompile
import PythonCompile

import System.Environment

compile compiler i = let (res, _) = parse getAssignments $ makeParseString i in
    case res of
        Just r -> compiler r
        Nothing -> "Failure"

main = do
    inputFileName <- fmap (head) getArgs 
    input <- readFile inputFileName
    output <- writeFile "output.lua" $ compile compileToLua input
    output <- writeFile "output.py" $ compile compileToPython input
    return ()
