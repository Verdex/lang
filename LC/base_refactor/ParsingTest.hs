
module ParsingTest where

import Control.Applicative
import Parsing


p1 =
    do
        init <- getString "jabber"
        next <- (getString "wocky") <|> (fmap show getAnyDigit) 
        even <- some getAnyDigit 
        getString "|"
        odds <- many getAnyDigit
        getString "|"
        return ()

p2 = 
    do
        (some p1) <?> "cat"
        end
        return ()

combinator p = 
    do
        (some p) <?> "some name"
        return ()

p3 = combinator p2

fail1 = parseWith p3 (makeParseString "jabber556|")
