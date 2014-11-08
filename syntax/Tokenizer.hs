
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
        ts <- many $ getSpaces
                  <|> getNewLine  
                  <|> getSymbol
        end
        return ts

getSymbol =
    do
        f <- underscore <|> getAnyAlpha
        r <- many $ underscore 
                <|> getAnyAlpha 
                <|> (fmap intToDigit getAnyDigit)
                <|> getString "'"
        return $ Symbol (f:r)

    where underscore = fmap head (getString "_")


getSimple :: String -> Token -> Parser String Token
getSimple s t =
    do 
        getString s
        return t

getNewLine = (getSimple "\r\n" NewLine) <|> (getSimple "\r" NewLine) <|> (getSimple "\n" NewLine)

getSpaces = fmap (LangAst.Space . length) $ some $ getString " "
