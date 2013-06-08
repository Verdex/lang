
module Main where

import Text.Parsec.Prim
import Parser

testSimple input = runParser simpleType () "" input

simple01 = testSimple "Type"
simple02 = testSimple "type"

main = print [simple01, simple02] 
