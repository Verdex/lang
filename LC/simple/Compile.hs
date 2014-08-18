
module Main where

import Parse
import ParseLang
import ParseAst
import LuaCompile
import PythonCompile
import RubyCompile

import System.Environment

compile compiler i = let result = parse getAssignments $ makeParseString i in
    case result of
        Success r _ -> compiler r
        Failure _ -> "Failure"

main = do
    inputFileName <- fmap (head) getArgs 
    input <- readFile inputFileName
    writeFile "output.lua" $ compile compileToLua input
    writeFile "output.py" $ compile compileToPython input
    writeFile "output.rb" $ compile compileToRuby input
    return ()
