
module Parser where

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim


simpleType :: Parsec String () Func 
simpleType =  
    do 
        fl <- upper
        r <- many (alphaNum <|> oneOf ['`', '_']) 
        return (Simp $ fl : r)

spaceArrow :: Parsec String () () -- might be able to use adjoint
spaceArrow =
    do
        optional spaces
        string "->"
        optional spaces
        return ()
-- TODO move to another file (consider refactor)
type TypeName = String
data Func = Paren Func
    | Arrow Func Func
    | Simp TypeName
    deriving Show

case1 :: Parsec String () Func 
case1 = 
    do 
        char '('
        t <- typeP
        char ')'
        return (Paren t)

case2 :: Parsec String () Func
case2 = 
    do 
        t1 <- typeP
        spaceArrow
        t2 <- typeP
        return (Arrow t1 t2)

-- TODO loops forever? ... left recursion?
typeP :: Parsec String () Func
typeP = case1 <|> case2 <|> simpleType

