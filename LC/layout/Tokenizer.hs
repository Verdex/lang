
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

