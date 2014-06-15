
module Main where

import Parse
import ParseLang
import ParseAst
import LuaCompile
import Control.Applicative

test term lua = 
    let (Just a, o) = parse getLambdaTerm (makeParseString term)
        luaRes = stringifyToLua [] a in
    case lua == luaRes of
        True -> "Pass " ++ lua
        False -> "Fail - Found " ++ lua ++ ":: Should be " ++ luaRes

tests = 
    [ test "\\ a . a" "( function ( a ) return a end )"
    , test "\\ a . a a" "( function ( a ) return ( a )( a ) end )"
    ]

main = mapM_ putStrLn tests
