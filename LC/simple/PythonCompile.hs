
module PythonCompile where

import ParseAst


compileToPython as = "def create(): \n\
\   funcs = {} \n\
\" ++ setupAssignments as ++ "\
\   return funcs \n\
\ "

stringifyToPython env (Var name) 
    | any (== name) env = name 
    | otherwise = "topLevel['" ++ name ++ "']"
stringifyToPython env (Abs param expr) = 
    "( lambda " ++ param ++ " : " ++ stringifyToPython (param : env) expr ++ " )" 
stringifyToPython env (App e1 e2) = 
    "( " ++ stringifyToPython env e1 ++ " )( " ++ stringifyToPython env e2 ++ " )" 

setupAssignments as = foldr (++) "" (map (loadSetup . convert) as)

    where convert (Assignment name expr) = (name, 
            "lambda topLevel : " ++ stringifyToPython [] expr )

          loadSetup (name, func) = 
            "   funcs['" ++ name ++ "'] = ( " ++ func ++ " )( funcs )\n"


