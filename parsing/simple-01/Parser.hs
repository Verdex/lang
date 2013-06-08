
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

ikky :: Parsec String () String
ikky = 
    do 
        t1 <- simpleType
        -- optional spaces
        -- optional arrow
        -- optional spaces
        -- t2 <- simple type
        -- recurse?

