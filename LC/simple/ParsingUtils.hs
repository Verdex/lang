
module ParsingUtils where

import Control.Applicative
import Data.Char
import Parsing


getSymbol = 
    do
        first <- underscore <|> getAnyAlpha
        rest <- many $ underscore <|> getAnyAlpha <|> fmap intToDigit getAnyDigit
        return $ first : rest

    where underscore = fmap head $ getString "_"


withParens parser =
    do
        getString "("
        many getWhiteSpace
        value <- parser 
        many getWhiteSpace
        getString ")"
        return value
