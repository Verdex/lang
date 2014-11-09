
module ParsingTerm where

import Control.Applicative
import Parsing
import ParsingUtils
import LangAst


getLambdaTerm = getApp <|> getVar <|> getAbs <|> getParen

getVar = fmap Var getSymbol

getAbs = do
    getString "\\"
    many getWhiteSpace
    var <- getSymbol
    many getWhiteSpace
    getString "."
    many getWhiteSpace
    expr <- getLambdaTerm 
    return $ Abs var expr

getParen = withParens getLambdaTerm

getApp = do
    e <- getShortLambdaTerm 
    es <- some getAppList
    return $ buildApp e es 

    where getAppList = do
            many getWhiteSpace
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
    end
    return assignments
