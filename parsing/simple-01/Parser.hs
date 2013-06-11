
module Parser where

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim

-- TODO move to another file (consider refactor)
type TypeName = String
data Func = Paren Func
    | Arrow Func Func
    | Simp TypeName
    deriving Show

spaceArrow :: Parsec String () () -- might be able to use adjoint
spaceArrow =
    do
        optional spaces
        string "->"
        optional spaces
        return ()

simpleType :: Parsec String () Func 
simpleType =  
    do 
        fl <- upper
        r <- many (alphaNum <|> oneOf ['`', '_']) 
        return (Simp $ fl : r)


parenType :: Parsec String () Func 
parenType = 
    do 
        char '('
        t <- typeParser
        char ')'
        return (Paren t)

funcType :: Parsec String () Func
funcType = 
    do 
        t1 <- simpleType 
        t2 <- tList 
        return (Arrow t1 t2)

tList :: Parsec String () Func
tList = optional jabber 

jabber :: Parsec String () Func
jabber = 
    do 
        spaceArrow
        t <- typeParser
        return t

-- TODO loops forever? ... left recursion?
typeParser :: Parsec String () Func
typeParser = parenType <|> funcType <|> simpleType

