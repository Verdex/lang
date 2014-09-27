
module Scoper where

import Control.Applicative
import Parsing
import LangAst

-- put a new line on the front of the input token stream so that symbol defines at line zero are uniform
scope :: [Token] -> [Scope]
scope toks = case parseWith getScopesWithEnd (makeParseSource $ NewLine : toks) 
             of
                Success s _ -> s
                Failure -> []


getToken tok = getAnyX (tok ==) id

getAnySpace = getAnyX anySpace id 
    where anySpace (Space _) = True
          anySpace _ = False

getAnySymbol = getAnyX anySymbol id 
    where anySymbol (Symbol _) = True
          anySymbol _ = False

getAnyToken = getAnyX (const True) id

zeroScopedAssign = 
    do
        some $ getToken NewLine
        sym <- getAnySymbol
        zeroOrOne getAnySpace
        getToken Assign
        return sym

getScope =
    do
        (Symbol n) <- zeroScopedAssign
        ts <- getAnyToken `parseUntil` zeroScopedAssign
        return $ Scope n ts

getScopesWithEnd =
    do
        scopes <- many getScope
        end
        return scopes

