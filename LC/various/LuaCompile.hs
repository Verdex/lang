
module LuaCompile where

import ParseAst


compileToLua = undefined

stringifyToLua env (Var name) 
    | any (== name) env = name 
    | otherwise = "topLevel[\"" ++ name ++ "\"]"
stringifyToLua env (Abs param expr) = 
    "( function ( " ++ param ++ " ) return " ++ stringifyToLua (param : env) expr ++ " end )" 
stringifyToLua env (App e1 e2) = 
    "( " ++ stringifyToLua env e1 ++ " )( " ++ stringifyToLua env e2 ++ " )" 