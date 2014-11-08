
module ParseLang where

import Parse
import ParseAst
import Control.Applicative
import Data.Char

getSymbol = do
    f <- underscore <|> getAnyLetter
    r <- many $ underscore <|> getAnyLetter <|> fmap intToDigit getAnyDigit
    return $ f : r

    where underscore = fmap head $ getString "_"

getVar = fmap Var getSymbol

getLambdaTerm = getApp <|> getVar <|> getAbs <|> getParen

getAbs = do
    getString "\\"
    many getWhiteSpace
    var <- getSymbol
    many getWhiteSpace
    getString "."
    many getWhiteSpace
    expr <- getLambdaTerm 
    return $ Abs var expr

getParen = do
    getString "("
    many getWhiteSpace
    expr <- getLambdaTerm
    many getWhiteSpace 
    getString ")"
    return expr

getApp = do
    e <- getShortLambdaTerm 
    es <- some getAppList
    return $ buildApp e es 

    where getAppList = do
            some getWhiteSpace <|> 
                do
                    many getWhiteSpace
                    lookAhead (getString ")") <|> lookAhead (getString "(")
            e <- getShortLambdaTerm
            return e
            
          getShortLambdaTerm = getVar <|> getAbs <|> getParen
     
          buildApp e' (e:[]) = App e' e
          buildApp e' (e:es) = buildApp (App e' e) es


getAssignment = do
    name <- getSymbol
    many getWhiteSpace
    getString "="
    many getWhiteSpace
    body <- getLambdaTerm
    many getWhiteSpace
    getString ";"
    return $ Assignment name body
   
getAssignments = do
    assignments <- many $
        do
            many getWhiteSpace
            assignment <- getAssignment
            many getWhiteSpace
            return assignment
    endStream
    return assignments
