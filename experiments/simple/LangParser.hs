
module LangParser where

-- TODO only export parse

import Control.Applicative
import Parsing
import LangAst


parse :: [Token] -> Program
parse = undefined


pTypeDef :: Parser [Token] TypeDef 
pTypeDef =
    do
        pSimple Data
        typeName <- pAnySymbol
        pSimple Assign
        -- get many cons + param until ; (and | in between)
        return $ TypeDef { td_name = typeName
                         , td_cons = []
                         }   


pAnySymbol :: Parser [Token] String
pAnySymbol = getAnyX matchSymbol projSymbol

    where matchSymbol t = case t 
                          of
                              Symbol _ -> True
                              _ -> False

          projSymbol (Symbol n) = n

pSimple :: Token -> Parser [Token] () 
pSimple tok = getAnyX (\ v -> v == tok) (const ())

