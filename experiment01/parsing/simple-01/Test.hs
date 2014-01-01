
module Main where

import Text.Parsec.Prim
import Parser

testSimple input = runParser simpleType () "" input
testFuncType input = runParser typeParser () "" input

simple01 = testSimple "Type"
simple02 = testSimple "type"
simple03 = testSimple "BlA76_`"

func01 = testFuncType "Type;"
func02 = testFuncType "(Type -> TyPe_01`) -> Int ;"

main = print [func01,func02]
