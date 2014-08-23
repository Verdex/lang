
module ParsingUtils where

import Control.Applicative
import Data.Char
import Parsing


underscore = fmap head $ getString "_"

getTypeSymbol =
    do
        first <- getAnyAlpha
        assert (isUpper first)
        rest <- many $ underscore <|> getAnyAlpha <|> fmap intToDigit getAnyDigit
        return $ first : rest

getSymbol = 
    do
        first <- underscore <|> getAnyAlpha
        rest <- many $ underscore <|> getAnyAlpha <|> fmap intToDigit getAnyDigit
        return $ first : rest

withParens parser =
    do
        getString "("
        many getWhiteSpace
        value <- parser 
        many getWhiteSpace
        getString ")"
        return value
