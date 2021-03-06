
-- Note that this tokenizer isn't going to work with where, let, or match
-- In order to handle that I'm going to need column location
-- Just adding in line number and column number location probably solves that problem

module Tokenizer (tokenize) where

import Control.Applicative
import Parsing
import LangAst
import Data.Char

tokenize :: String -> [Token]
tokenize str = 
    case parseWith allTokens (makeParseSource str) 
    of
        Success ts _ -> ts
        Failure -> []

allTokens =
    do
        ts <- many $ getSymbol
                  <|> getArrow
                  <|> getLParen
                  <|> getRParen
                  <|> getAssign
                  <|> getLambda
                  <|> getNewLine
                  <|> getSpaces 
        end
        return ts

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
