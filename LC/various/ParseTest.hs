
module Main where

import Parse
import Control.Applicative


test parser input result = let o = parse parser input in
    case result == o of
        True -> "Pass " ++ show o
        False -> "Fail " ++ show o

i = makeParseString

tests = 
    [ test (getString "blah") (i "blah") (Just "blah", (4, "blah"))
    , test (getString "a") (i "aa") (Just "a", (1, "aa"))
    , test (getString "a" <|> getString "b") (i "b") (Just "b", (1, "b"))
    , test (getString "a" <|> getString "b") (i "a") (Just "a", (1, "a"))
    , test (getString "a" <|> getString "b") (i "c") (Nothing, (0, "c"))
    , test (some $ getString "a") (i "a") (Just ["a"], (1, "a"))
    , test (some $ getString "a") (i "aaa") (Just ["a", "a", "a"], (3, "aaa"))
    , test (some $ getString "a") (i "") (Nothing, (0, ""))
    , test (many $ getString "a") (i "") (Just [], (0, ""))
    , test (many $ getString "a") (i "aa") (Just ["a","a"], (2, "aa"))
    , test (many $ getString "a" <|> getString "b") (i "abbac") (Just ["a","b", "b", "a"], (4, "abbac"))
    , test ((many $ getString "a" <|> getString "b") >> endStream) (i "abbac") (Nothing, (4, "abbac"))
    , test ((many $ getString "a" <|> getString "b") >> endStream) (i "abba") 
            (Just (), (4, "abba"))
    ]

main = mapM_ putStrLn tests
