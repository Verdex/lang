
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

getAbs = do
    getString "\\"
    many getWhiteSpace
    var <- getSymbol
    many getWhiteSpace
    getString "."
    many getWhiteSpace
    expr <- getLambdaTerm 
    return (Abs var expr)

getApp = undefined

getParen = do
    getString "("
    many getWhiteSpace
    expr <- getLambdaTerm
    many getWhiteSpace 
    getString ")"
    return expr

getLambdaTerm = getApp <|> getVar <|> getAbs <|> getParen
