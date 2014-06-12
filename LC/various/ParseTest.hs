
module Main where

import Data.List
import Parse


test parser input result = let o = parse parser input in
    case result == o of
        True -> "Pass " ++ show o
        False -> "Fail " ++ show o

i = makeParseString

tests = 
    [ test (getString "blah") (i "blah") (Just "blah", (4, "blah"))
    , test (getString "a") (i "aa") (Just "a", (1, "aa"))
    ]

main = mapM_ putStrLn tests
