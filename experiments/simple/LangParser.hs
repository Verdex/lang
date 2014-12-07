
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
 
 -- data x = x ;
 -- data x = y ;
 -- data x = y | z ;
 -- data x = y | z | w ;
 -- data x = y a ;
 -- data x = y a b ;
 -- data x = y a b c;
 -- data x = y x;
 -- data x = y a b | z a b c;
 -- data x = y a b | z a | w ;
 -- data x = y (a -> b) ;
 -- data x = y (a -> b) c | z d ;

 -- TODO will need type sig parser so I can get arbitrary types of constructor parameters parsed

pAnySymbol :: Parser [Token] String
pAnySymbol = getAnyX matchSymbol projSymbol

    where matchSymbol t = case t 
                          of
                              Symbol _ -> True
                              _ -> False

          projSymbol (Symbol n) = n

pSimple :: Token -> Parser [Token] () 
pSimple tok = getAnyX (\ v -> v == tok) (const ())

