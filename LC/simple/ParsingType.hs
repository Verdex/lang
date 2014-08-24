
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
        --(getString "\r\n") <|> (getString "\n") <|> (getString "\r")
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
