
module ParseLang where

import Parse
import ParseAst
import Control.Applicative
import Data.Char

getSymbol = do
    f <- underscore <|> getAnyLetter
    r <- many $ underscore <|> getAnyLetter <|> fmap intToDigit getAnyDigit
    return (f : r)

    where underscore = fmap head $ getString "_"

getVar = fmap Var getSymbol

    
