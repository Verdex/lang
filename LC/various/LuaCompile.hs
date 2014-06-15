
module LuaCompile where

import ParseAst


compileToLua = undefined

stringifyToLua (Var name) = name 
stringifyToLua (Abs param expr) = "( function ( " ++ param ++ " ) return " ++ stringifyToLua expr ++ " end )" 
stringifyToLua (App e1 e2) = "( " ++ stringifyToLua e1 ++ " )( " ++ stringifyToLua e2 ++ " )" 
