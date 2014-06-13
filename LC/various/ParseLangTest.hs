
module Main where

import Parse
import ParseLang
import ParseAst
import Control.Applicative


test parser input result = let o = parse parser input in
    case result == o of
        True -> "Pass " ++ show o
        False -> "Fail " ++ show o

i = makeParseString

tests = 
    [ test getSymbol (i "blah") (Just "blah", (4, "blah"))
    , test getSymbol (i "_blah") (Just "_blah", (5, "_blah"))
    , test getSymbol (i "_") (Just "_", (1, "_"))
    , test getSymbol (i "_1") (Just "_1", (2, "_1"))
    , test getSymbol (i "_blah1") (Just "_blah1", (6, "_blah1"))
    , test getSymbol (i "_bl ah1") (Just "_bl", (3, "_bl ah1"))
    , test getVar (i "_bl ah1") (Just $ Var "_bl", (3, "_bl ah1"))
    ]

main = mapM_ putStrLn tests

