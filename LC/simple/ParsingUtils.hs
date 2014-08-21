
module ParsingUtils where

import Control.Applicative
import Data.Char
import Parsing


getSymbol = 
    do
        first <- underscore <|> getAnyLetter
        rest <- many $ underscore <|> getAnyLetter <|> fmap intToDigit getAnyDigit
        return $ first : rest
