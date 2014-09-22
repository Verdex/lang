
module Tokenizer where

import Control.Applicative
import Parsing
import LangAst
import Data.Char

tokenize :: String -> [Token]
tokenize = undefined

getSymbol =
    do
        f <- underscore <|> getAnyAlpha
        r <- many $ underscore <|> getAnyAlpha <|> (fmap intToDigit getAnyDigit)
        return $ Symbol (f:r)

    where underscore = fmap head (getString "_")

getSimple :: String -> Token -> Parser String Token
getSimple s t =
    do 
        getString s
        return t

getArrow = getSimple "->" Arrow

getLParen = getSimple "(" LParen

getRParen = getSimple ")" RParen

getAssign = getSimple "=" Assign

getLambda = getSimple "\\" Lambda

getNewLine = (getSimple "\r\n" NewLine) <|> (getSimple "\r" NewLine) <|> (getSimple "\n" NewLine)

getSpaces = fmap (LangAst.Space . length) $ some $ getString " "
