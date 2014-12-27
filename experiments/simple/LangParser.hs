
module LangParser where

-- TODO only export parse

import Control.Applicative
import Parsing
import LangAst


parse :: [Token] -> Program
parse = undefined


typeDef :: Parser [Token] TypeDef 
typeDef = 
    do
        literally LangAst.Data ()
        name <- anySymbol
        literally LangAst.Assign ()
        cs <- consList
        return $ TypeDef { typedef_name = name
                         , typedef_cons = cs
                         }

    where consDef =
            do
                consName <- anySymbol
                params <- many typeSig
                return $ Cons { cons_name = consName
                              , cons_params = params
                              }
          consListNode =
            do
                literally LangAst.OrBar ()
                c <- consDef
                return c

          consList =
            do
                c <- zeroOrOne consDef
                cs <- many consListNode
                literally LangAst.Semicolon ()
                return $ comb c cs

          comb (Just c) cs = c : cs
          comb Nothing cs = cs


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


typeSig :: Parser [Token] TypeSig
typeSig = arrow <|> paren <|> tsym 

    where tsym = 
            do
                name <- anySymbol
                return $ TSingle name
          arrow = 
            do 
                a <- paren <|> tsym 
                literally LangAst.RArrow ()
                b <- typeSig
                return $ TArrow a b

          paren =
            do
                literally LangAst.LParen ()
                t <- typeSig
                literally LangAst.RParen ()
                return t


anySymbol :: Parser [Token] String
anySymbol = matchSymbol ?=> projSymbol

    where matchSymbol (Symbol _) = True
          matchSymbol _ = False
          projSymbol (Symbol n) = n


