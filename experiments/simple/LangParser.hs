
module LangParser where

-- TODO only export parse

import Control.Applicative
import Parsing
import LangAst


parse :: [Token] -> Program
parse = undefined

expr :: Parser [Token] Expr
expr = fmap EVar anySymbol
   <|> letExpr

matchExpr :: Parser [Token] Expr
matchExpr = undefined

letExpr :: Parser [Token] Expr
letExpr = 
    do
        literally Let ()
        name <- anySymbol
        literally Colon ()
        sig <- typeSig
        literally Assign ()
        assign <- expr
        literally In ()
        body <- expr
        return $ ELet { let_name = name
                      , let_sig = sig
                      , let_assign = assign
                      , let_body = body
                      }


typeDef :: Parser [Token] TypeDef 
typeDef = 
    do
        literally Data ()
        name <- anySymbol
        literally Assign ()
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
                literally OrBar ()
                c <- consDef
                return c

          consList =
            do
                c <- zeroOrOne consDef
                cs <- many consListNode
                literally Semicolon ()
                return $ comb c cs

          comb (Just c) cs = c : cs
          comb Nothing cs = cs


typeSig :: Parser [Token] TypeSig
typeSig = arrow <|> paren <|> tsym 

    where tsym = 
            do
                name <- anySymbol
                return $ TSingle name
          arrow = 
            do 
                a <- paren <|> tsym 
                literally RArrow ()
                b <- typeSig
                return $ TArrow a b

          paren =
            do
                literally LParen ()
                t <- typeSig
                literally RParen ()
                return t


anySymbol :: Parser [Token] String
anySymbol = matchSymbol ?=> projSymbol

    where matchSymbol (Symbol _) = True
          matchSymbol _ = False
          projSymbol (Symbol n) = n


