
module LuaCompile where

import ParseAst


compileToLua as = "function create() \n \
\ local funcs = {} \n \
\ " ++ setupAssignments as ++ " \
\ end"

stringifyToLua env (Var name) 
    | any (== name) env = name 
    | otherwise = "topLevel[\"" ++ name ++ "\"]"
stringifyToLua env (Abs param expr) = 
    "( function ( " ++ param ++ " ) return " ++ stringifyToLua (param : env) expr ++ " end )" 
stringifyToLua env (App e1 e2) = 
    "( " ++ stringifyToLua env e1 ++ " )( " ++ stringifyToLua env e2 ++ " )" 

setupAssignments as = foldr append "" (map (loadSetup . convert) as)

    where convert (Assignment name expr) = (name, 
            "return function( topLevel ) return " ++ stringifyToLua [] expr ++ " end")

          loadSetup (name, func) = 
            "funcs[\"" ++ name ++ "\"] = load( \"" ++ func ++ "\" )()( funcs )\n"

          append line prev = prev ++ line ++ "\n"


