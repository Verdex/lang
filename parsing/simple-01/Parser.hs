
module Parser where

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim


simpleType :: Parsec String () String
simpleType =  
    do 
        fl <- upper
        r <- many (alphaNum <|> oneOf ['`', '_']) 
        return (fl : r)

spaceArrow :: Parsec String () ()
spaceArrow =
    do
        optional spaces
        string "->"
        optional spaces
        return ()

jabber :: Parsec String () [String]
jabber = sepBy1 simpleType spaceArrow


