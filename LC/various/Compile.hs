
module Main where

import Parse
import ParseLang
import ParseAst
import LuaCompile

compile i = let (res, _) = parse getAssignments $ makeParseString i in
    case res of
        Just r -> compileToLua r
        Nothing -> "Failure"

main = do
    input <- readFile "input.lc"
    output <- writeFile "output.lua" $ compile input
    return ()
