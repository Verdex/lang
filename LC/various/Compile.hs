
module Main where

import Parse
import ParseLang
import ParseAst
import LuaCompile
import PythonCompile
import RubyCompile

import System.Environment

compile compiler i = let (res, _) = parse getAssignments $ makeParseString i in
    case res of
        Just r -> compiler r
        Nothing -> "Failure"

main = do
    inputFileName <- fmap (head) getArgs 
    input <- readFile inputFileName
    writeFile "output.lua" $ compile compileToLua input
    writeFile "output.py" $ compile compileToPython input
    writeFile "output.rb" $ compile compileToRuby input
    return ()
