
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

fail1 = parseWith p2 (makeParseString "jabber556||")
