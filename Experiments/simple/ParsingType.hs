
module ParsingType where

import Control.Applicative
import Parsing
import ParsingUtils
import LangAst


getType = getTypeArrow <|> getTypeName <|> getTypeParen

getTypeName = fmap TypeName getTypeSymbol

getTypeArrow =  
    do
        init <- typeAtom
        rest <- many typeArrow 
        return $ buildArrow init rest
    

    where typeAtom = getTypeName <|> getTypeParen
        
          typeArrow = 
              do
                  many getWhiteSpace
                  getString "->"
                  many getWhiteSpace
                  t <- typeAtom
                  return t

          buildArrow i [] = i
          buildArrow i (r:rs) = TypeArrow i (buildArrow r rs)

getTypeParen = withParens getType

getDataDefinition = 
    do
        getString "data"
        many getWhiteSpace
        typeName <- getTypeSymbol
        many getWhiteSpace
        getString "="
        constructors <- (fmap (:[]) cons)  <|> consList
        getString ";"
        return DataDef { typeName = typeName
                       , cons = constructors 
                       }

    where consList = 
            do
                ci <- cons  <?> "consList initial"
                many getWhiteSpace
                getString "|"
                cs <- consList <?> "consList list"
                cl <- cons <?> "consList last"
                many getWhiteSpace
                return $ ci : cs ++ [cl]
    
          cons =
            do
                many getWhiteSpace
                name <- getTypeSymbol <?> "cons get type symbol"
                types <- typeList <?> "cons typeList"
                return ConsDef { consName = name
                               , parameters = types 
                               }
          typeList = some $
            do
                many getWhiteSpace
                t <- getType <?> "typeList getType"
                return t

