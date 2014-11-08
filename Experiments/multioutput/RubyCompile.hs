
module RubyCompile where

import ParseAst


compileToRuby as = "def create \n \
\ funcs = {} \n \
\ " ++ setupAssignments as ++ " \
\ funcs \n \
\ end"

stringifyToRuby env (Var name) 
    | any (== name) env = name 
    | otherwise = "topLevel['" ++ name ++ "']"
stringifyToRuby env (Abs param expr) = 
    "( -> " ++ param ++ " { " ++ stringifyToRuby (param : env) expr ++ " } )" 
stringifyToRuby env (App e1 e2) = 
    "( " ++ stringifyToRuby env e1 ++ " )[ " ++ stringifyToRuby env e2 ++ " ]" 

setupAssignments as = foldr (++) "" (map (loadSetup . convert) as)

    where convert (Assignment name expr) = (name, 
            "-> topLevel { " ++ stringifyToRuby [] expr ++ " }")

          loadSetup (name, func) = 
            "funcs['" ++ name ++ "'] = ( " ++ func ++ " )[ funcs ]\n"


