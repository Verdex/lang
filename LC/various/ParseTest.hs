
module Main where

import Parse

main = print $ parse (getString "blah") (makeParseString "blah")
